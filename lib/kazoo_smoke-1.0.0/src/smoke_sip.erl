%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% SIP-related helpers
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(smoke_sip).

-export([version/0
         ,parse_sip_packet/1

         ,extract_method/1
         ,extract_sip_uri/1

         ,headers_supported/0
         ,methods_supported/0
         ,transports_supported/0
         ,is_known_method/1
         ,is_known_transport/1
         ,format_response_code/1
        ]).

-compile([export_all]).

-include("smoke.hrl").
-include("smoke_sip.hrl").

-export_type([sip_method/0
              ,sip_version/0
              ,sip_header/0
              ,sip_headers/0
              ,sip_status/0
              ,sip_transport/0
             ]).

-spec version/0 :: () -> sip_version().
version() ->
    'SIP/2.0'.

-spec headers_supported/0 :: () -> [sip_header()].
headers_supported() -> ?HEADERS_SUPPORTED.

-spec methods_supported/0 :: () -> [sip_method()].
methods_supported() -> ?METHODS_SUPPORTED.

-spec transports_supported/0 :: () -> [sip_transport()].
transports_supported() -> ?TRANSPORTS_SUPPORTED.

-spec is_known_method/1 :: (atom() | string() | binary()) -> 'false' | {'true', sip_method()}.
is_known_method(M) ->
    MBin = wh_util:to_upper_binary(wh_util:to_binary(M)),
    case catch wh_util:to_atom(MBin) of
        {'EXIT', _} -> false;
        Matom when is_atom(Matom) ->
            case lists:member(Matom, ?METHODS_SUPPORTED) of
                true -> {true, Matom};
                false -> false
            end
    end.

-spec is_known_transport/1 :: (atom() | string() | binary()) -> 'false' | {'true', sip_transport()}.
is_known_transport(T) ->
    case catch wh_util:to_atom(T) of
        {'EXIT', _} -> false;
        Tatom when is_atom(Tatom) ->
            case lists:member(Tatom, ?TRANSPORTS_SUPPORTED) of
                true -> {true, Tatom};
                false -> false
            end
    end.

-spec parse_sip_packet/1 :: (binary()) ->
                                    {'ok', sip_req()} |
                                    {'error', sip_response_code()}.
-spec parse_sip_packet/2 :: (binary(), sip_method()) ->
                                    {'ok', sip_req()} |
                                    {'error', sip_response_code()}.
-spec parse_sip_packet/3 :: (binary(), sip_method(), sip_uri()) ->
                                    {'ok', sip_req()} |
                                    {'error', sip_response_code()}.
-spec parse_sip_packet/4 :: (binary(), sip_method(), sip_uri(), sip_version()) ->
                                    {'ok', sip_req()} |
                                    {'error', sip_response_code()}.
parse_sip_packet(Buffer) when is_binary(Buffer) ->
    case extract_method(Buffer) of
        {error, _}=E -> E;
        {M, Buffer1} -> parse_sip_packet(Buffer1, M)
    end.
parse_sip_packet(Buffer, M) ->
    case extract_sip_uri(Buffer) of
        {error, _}=E -> E;
        {RequestUri, Buffer1} -> parse_sip_packet(Buffer1, M, RequestUri)
    end.
parse_sip_packet(Buffer, M, RUri) ->
    case extract_sip_version(Buffer) of
        {error, _}=E -> E;
        {Vsn, Buffer1} -> parse_sip_packet(Buffer1, M, RUri, Vsn)
    end.
parse_sip_packet(Buffer, M, #sip_uri{user=U, host=H}=RUri, _Vsn) ->
    lager:info("recv ~s request for: ~s@~s", [M, U, H]),

    HeadersToSet = parse_headers(Buffer),

    lists:foldl(fun({F, D}, R) -> smoke_sip_req:F(R, D) end
                ,smoke_sip_req:new()
                ,[{set_method, M}
                  ,{set_request_uri, RUri}
                  | HeadersToSet
                 ]).

-spec parse_headers/1 :: (binary()) -> [{sip_header() | ne_binary(), ne_binary() | integer()}].
parse_headers(Buffer) ->
    case extract_until(Buffer, <<"\r\n\r\n">>, ignore_eol) of
        {terminator, Headers, Body} ->
            lager:debug("headers: ~s", [Headers]),
            lager:debug("body: ~s", [Body]),
            [];
        {eof, Headers, _} ->
            lager:debug("headers: ~s", [Headers]),
            lager:debug("no body"),
            []
    end.

extract_sip_version(<<"SIP/2.0", Buffer/binary>>) ->
    {version(), Buffer};
extract_sip_version(_) ->
    {error, 505}.

-spec extract_method/1 :: (ne_binary()) -> {sip_method(), binary()} |
                                           {'error', 501}.
extract_method(<<"INVITE ", Buffer/binary>>)    -> {'INVITE', Buffer};
extract_method(<<"ACK ", Buffer/binary>>)       -> {'ACK', Buffer};
extract_method(<<"BYE ", Buffer/binary>>)       -> {'BYE', Buffer};
extract_method(<<"CANCEL ", Buffer/binary>>)    -> {'CANCEL', Buffer};
extract_method(<<"REGISTER ", Buffer/binary>>)  -> {'REGISTER', Buffer};
extract_method(<<"OPTIONS ", Buffer/binary>>)   -> {'OPTIONS', Buffer};
extract_method(<<"SUBSCRIBE ", Buffer/binary>>) -> {'SUBSCRIBE', Buffer};
extract_method(<<"NOTIFY ", Buffer/binary>>)    -> {'NOTIFY', Buffer};
extract_method(<<"UPDATE ", Buffer/binary>>)    -> {'UPDATE', Buffer};
extract_method(<<"MESSAGE ", Buffer/binary>>)   -> {'MESSAGE', Buffer};
extract_method(<<"REFER ", Buffer/binary>>)     -> {'REFER', Buffer};
extract_method(<<"INFO ", Buffer/binary>>)      -> {'INFO', Buffer};
extract_method(_) -> {error, 501}.

-spec extract_sip_uri/1 :: (ne_binary()) -> {sip_uri(), ne_binary()} |
                                            {'error', 400}.
extract_sip_uri(<<"sip:", Buffer/binary>>) ->
    extract_sip_uri(Buffer, 'sip');
extract_sip_uri(<<"sips:", Buffer/binary>>) ->
    extract_sip_uri(Buffer, 'sips');
extract_sip_uri(_B) ->
    {error, 400}.

-spec extract_sip_uri/2 :: (ne_binary(), 'sip' | 'sips') -> {sip_uri(), ne_binary()} |
                                                            {'error', integer()}.
extract_sip_uri(Buffer, Scheme) ->
    case binary:split(Buffer, <<"@">>) of
        [UP, Rest] ->
            case extract_sip_user_pass(UP) of
                {U, P, _} -> extract_sip_host_port(Scheme, decode_uri(U), P, Rest);
                {error, _}=E -> E
            end;
        [Rest] ->
            extract_sip_host_port(Scheme, undefined, undefined, Rest)
    end.

extract_sip_host_port(Scheme, User, Pass, Buffer) ->
    case extract_sip_host_port(Buffer) of
        {error, _}=E -> E;
        {H, Port, Buffer1} ->
            extract_sip_params_headers(Scheme, User, Pass, H, Port, Buffer1)
    end.

extract_sip_params_headers(Scheme, User, Pass, Host, Port, Buffer) ->
    case extract_sip_params_headers(Buffer) of
        {error, _}=E -> E;
        {Params, Hdrs, Buffer1} ->
            {#sip_uri{scheme=Scheme
                      ,user=User
                      ,password=Pass
                      ,host=wh_util:to_lower_binary(Host)
                      ,port=Port
                      ,params=Params
                      ,headers=Hdrs
                     }
             ,Buffer1}
    end.

-spec extract_sip_user_pass/1 :: (ne_binary()) -> {ne_binary(), binary(), ne_binary()}.
extract_sip_user_pass(Buffer) ->
    extract_sip_user(Buffer, []).

-spec extract_sip_user/2 :: (ne_binary(), list()) -> {ne_binary(), binary(), ne_binary()}.
extract_sip_user(<<>>, Acc) ->
    %% no password to extract
    {iolist_to_binary(lists:reverse(Acc)), undefined, <<>>};
extract_sip_user(<<":", Buffer/binary>>, Acc) ->
    %% have a password to extract
    User = iolist_to_binary(lists:reverse(Acc)),
    {Pass, Buffer1} = extract_sip_password(Buffer),
    {User, Pass, Buffer1};
extract_sip_user(<<"@", Buffer/binary>>, Acc) ->
    %% no password, at host boundry
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_user(<<U:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_user(Buffer, [U | Acc]).

-spec extract_sip_password/1 :: (ne_binary()) -> {binary(), ne_binary()}.
-spec extract_sip_password/2 :: (ne_binary(), list()) -> {binary(), ne_binary()}.
extract_sip_password(Buffer) ->
    extract_sip_password(Buffer, []).
extract_sip_password(<<>>, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), <<>>};
extract_sip_password(<<"@", Buffer/binary>>, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), Buffer};
extract_sip_password(<<P:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_password(Buffer, [P | Acc]).

-spec extract_sip_host_port/1 :: (ne_binary()) -> {ne_binary(), binary(), ne_binary()}.
extract_sip_host_port(Buffer) ->
    extract_sip_host(Buffer, []).

extract_sip_host(<<>>, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), undefined, <<>>};
extract_sip_host(<<":", Buffer/binary>>, Acc) ->
    % have port to extract
    {Port, Buffer1} = extract_sip_port(Buffer),
    {iolist_to_binary(lists:reverse(Acc)), Port, Buffer1};
extract_sip_host(<<";", Buffer/binary>>, Acc) ->
    % no port, at host params boundry
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_host(<<" ", Buffer/binary>>, Acc) ->
    % no port, at host params boundry
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_host(<<"\r\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_host(<<"\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_host(<<H:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_host(Buffer, [H | Acc]).

-spec extract_sip_port/1 :: (ne_binary()) -> {pos_integer(), ne_binary()}.
-spec extract_sip_port/2 :: (ne_binary(), list()) -> {pos_integer(), ne_binary()}.
extract_sip_port(Buffer) ->
    extract_sip_port(Buffer, []).
extract_sip_port(<<";", Buffer/binary>>, Acc) ->
    % at params boundry
    {wh_util:to_integer(lists:reverse(Acc)), Buffer};
extract_sip_port(<<" ", Buffer/binary>>, Acc) ->
    % at end of URI
    {wh_util:to_integer(lists:reverse(Acc)), Buffer};
extract_sip_port(<<"\r\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {wh_util:to_integer(lists:reverse(Acc)), Buffer};
extract_sip_port(<<"\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {wh_util:to_integer(lists:reverse(Acc)), Buffer};
extract_sip_port(<<P:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_port(Buffer, [P | Acc]).

-spec extract_sip_params_headers/1 :: (ne_binary()) -> {ne_binary(), binary(), ne_binary()}.
extract_sip_params_headers(Buffer) ->
    extract_sip_params(Buffer, #sip_uri_params{}).

extract_sip_params(<<>>, Params) ->
    {Params, [], <<>>};
extract_sip_params(<<"?", Buffer/binary>>, Params) ->
    % at headers boundry
    {Hdrs, Buffer1} = extract_sip_headers(Buffer),
    {Params, Hdrs, Buffer1};
extract_sip_params(<<" ", Buffer/binary>>, Params) ->
    % no headers, at line ending
    {Params, [], Buffer};
extract_sip_params(<<"\r\n", Buffer/binary>>, Params) ->
    % no headers, at line ending
    {Params, [], Buffer};
extract_sip_params(<<"\n", Buffer/binary>>, Params) ->
    % no headers, at line ending
    {Params, [], Buffer};
extract_sip_params(<<"transport=", Buffer/binary>>, #sip_uri_params{transport=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    case is_known_transport(V) of
        {true, T} -> extract_sip_params(Buffer1, Params#sip_uri_params{transport=T});
        false -> {error, 400}
    end;
extract_sip_params(<<"maddr=", Buffer/binary>>, #sip_uri_params{maddr=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_params(Buffer1, Params#sip_uri_params{maddr=wh_util:to_lower_binary(V)});
extract_sip_params(<<"ttl=", Buffer/binary>>, #sip_uri_params{ttl=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_params(Buffer1, Params#sip_uri_params{ttl=wh_util:to_integer(V)});
extract_sip_params(<<"user=", Buffer/binary>>, #sip_uri_params{user=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_params(Buffer1, Params#sip_uri_params{user=wh_util:to_lower_binary(V)});
extract_sip_params(<<"lr=", Buffer/binary>>, #sip_uri_params{lr=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_params(Buffer1, Params#sip_uri_params{lr=wh_util:to_lower_binary(V)});
extract_sip_params(<<"method=", Buffer/binary>>, #sip_uri_params{method=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    case is_known_method(V) of
        {true, M} -> extract_sip_params(Buffer1, Params#sip_uri_params{method=M});
        false -> {error, 400}
    end;
extract_sip_params(Buffer, #sip_uri_params{other=Other}=Params) ->
    {Key, Buffer1} = extract_sip_param_key(Buffer),
    case props:get_value(Key, Other) of
        undefined ->
            {V, Buffer2} = extract_sip_param_value(Buffer1),
            extract_sip_params(Buffer2, Params#sip_uri_params{other=[{Key, V}|Other]});
        _V ->
            %% key has been defined, error!
            {error, 400}
    end.

-spec extract_sip_param_key/1 :: (ne_binary()) -> {ne_binary(), ne_binary()}.
extract_sip_param_key(Buffer) ->
    extract_sip_param_key(Buffer, []).
extract_sip_param_key(<<"=", Buffer/binary>>, Acc) ->
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_key(<<K:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_param_key(Buffer, [K | Acc]).

extract_sip_param_value(Buffer) ->
    extract_sip_param_value(Buffer, []).
extract_sip_param_value(<<";", Buffer/binary>>, Acc) ->
    % k/v delimiter
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_value(<<>>, Acc) ->
    {decode(lists:reverse(Acc)), <<>>};
extract_sip_param_value(<<"?", _/binary>> = Buffer, Acc) ->
    % end of params, start of headers
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_value(<<" ", _/binary>> = Buffer, Acc) ->
    % end of URI
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_value(<<"\r\n", _/binary>> = Buffer, Acc) ->
    % end of line
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_value(<<"\n", _/binary>> = Buffer, Acc) ->
    % end of line
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_value(<<V:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_param_value(Buffer, [V | Acc]).

extract_sip_headers(Buffer) ->
    extract_sip_headers(Buffer, [], []).

extract_sip_headers(<<>>, Hdrs, _) ->
    {Hdrs, <<>>};
extract_sip_headers(<<" ", _/binary>> = Buffer, Hdrs, _KeyAcc) ->
    {Hdrs, Buffer};
extract_sip_headers(<<"\r\n", _/binary>> = Buffer, Hdrs, _KeyAcc) ->
    {Hdrs, Buffer};
extract_sip_headers(<<"\n", _/binary>> = Buffer, Hdrs, _KeyAcc) ->
    {Hdrs, Buffer};
extract_sip_headers(<<"=", Buffer/binary>>, Hdrs, KeyAcc) ->
    %% Key ended, get value
    {Value, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_headers(Buffer1, [{decode(lists:reverse(KeyAcc)), Value}|Hdrs], []);
extract_sip_headers(<<K:1/binary, Buffer/binary>>, Hdrs, KeyAcc) ->
    extract_sip_headers(Buffer, Hdrs, [K | KeyAcc]).

%% Extracts until it hits either the terminator, end-of-line, or end-of-file,
%% and returns collected characters up to that point, and the leftover buffer
-spec extract_until/2 :: (binary(), byte() | ne_binary()) ->
                                 {'terminator' | 'eol' | 'eof', binary(), binary()}.
-spec extract_until/3 :: (binary(), byte() | ne_binary(), 'undefined' | 'ignore_eol') ->
                                 {'terminator' | 'eol' | 'eof', binary(), binary()}.
extract_until(Buffer, Terminator) ->
    extract_until(Buffer, Terminator, undefined, []).
extract_until(Buffer, Terminator, Opt) ->
    extract_until(Buffer, Terminator, Opt, []).

extract_until(<<Terminator, Buffer/binary>>, Terminator, _, Acc) ->
    {terminator, list_to_binary(lists:reverse(Acc)), Buffer};

extract_until(<<"\r\n", Buffer/binary>>, Terminator, ignore_eol, Acc) ->
    extract_until(Buffer, Terminator, ignore_eol, [<<"\n\r">> | Acc]);
extract_until(<<"\r\n", Buffer/binary>>, _, _, Acc) ->
    {eol, list_to_binary(lists:reverse(Acc)), Buffer};

extract_until(<<"\n", Buffer/binary>>, Terminator, ignore_eol, Acc) ->
    extract_until(Buffer, Terminator, ignore_eol, [<<"\n">> | Acc]);
extract_until(<<"\n", Buffer/binary>>, _, _, Acc) ->
    {eol, list_to_binary(lists:reverse(Acc)), Buffer};

extract_until(<<>>, _, _, Acc) ->
    {eof, list_to_binary(lists:reverse(Acc)), <<>>};
extract_until(<<T:1/binary, Buffer/binary>>, Terminator, Opt, Acc) ->
    extract_until(Buffer, Terminator, Opt, [T | Acc]).

-spec format_response_code/1 :: (sip_response_code()) -> ne_binary().
format_response_code(100) -> <<"Trying">>;
format_response_code(180) -> <<"Ringing">>;
format_response_code(181) -> <<"Call is Being Forwarded">>;
format_response_code(182) -> <<"Queued">>;
format_response_code(183) -> <<"Session in Progress">>;
format_response_code(199) -> <<"Early Dialog Terminated">>;

format_response_code(200) -> <<"OK">>;
format_response_code(202) -> <<"Accepted">>;
format_response_code(204) -> <<"No Notification">>;

format_response_code(300) -> <<"Multiple Choices">>;
format_response_code(301) -> <<"Moved Permanently">>;
format_response_code(302) -> <<"Moved Temporarily">>;
format_response_code(305) -> <<"Use Proxy">>;
format_response_code(380) -> <<"Alternative Service">>;

format_response_code(400) -> <<"Bad Request">>;
format_response_code(401) -> <<"Unauthorized">>;
format_response_code(402) -> <<"Payment Required">>;
format_response_code(403) -> <<"Forbidden">>;
format_response_code(404) -> <<"User not found">>;
format_response_code(405) -> <<"Method Not Allowed">>;
format_response_code(406) -> <<"Not Acceptable">>;
format_response_code(407) -> <<"Proxy Authentication Required">>;
format_response_code(408) -> <<"Request Timeout">>;
format_response_code(409) -> <<"Conflict">>;
format_response_code(410) -> <<"Gone">>;
format_response_code(412) -> <<"Conditional Request Failed">>;
format_response_code(413) -> <<"Request Entity Too Large">>;
format_response_code(414) -> <<"Request-URI Too Long">>;
format_response_code(415) -> <<"Unsupported Media Type">>;
format_response_code(416) -> <<"Unsupported URI Scheme">>;
format_response_code(417) -> <<"Unknown Resource-Priority">>;
format_response_code(420) -> <<"Bad Extension">>;
format_response_code(421) -> <<"Extension Required">>;
format_response_code(422) -> <<"Session Interval Too Small">>;
format_response_code(423) -> <<"Interval Too Brief">>;
format_response_code(424) -> <<"Bad Location Information">>;
format_response_code(428) -> <<"Use Identity Header">>;
format_response_code(429) -> <<"Provide Referrer Identity">>;
format_response_code(433) -> <<"Anonymity Disallowed">>;
format_response_code(436) -> <<"Bad Identity-Info">>;
format_response_code(437) -> <<"Unsupported Certificate">>;
format_response_code(438) -> <<"Invalid Identity Header">>;
format_response_code(480) -> <<"Temporarily Unavailable">>;
format_response_code(481) -> <<"Call/Transaction Does Not Exist">>;
format_response_code(482) -> <<"Loop Detected">>;
format_response_code(483) -> <<"Too Many Hops">>;
format_response_code(484) -> <<"Address Incomplete">>;
format_response_code(485) -> <<"Ambiguous">>;
format_response_code(486) -> <<"Busy Here">>;
format_response_code(487) -> <<"Request Terminated">>;
format_response_code(488) -> <<"Not Acceptable Here">>;
format_response_code(489) -> <<"Bad Event">>;
format_response_code(491) -> <<"Request Pending">>;
format_response_code(493) -> <<"Undecipherable">>;
format_response_code(494) -> <<"Security Agreement Required">>;

format_response_code(500) -> <<"Server Internal Error">>;
format_response_code(501) -> <<"Not Implemented">>;
format_response_code(502) -> <<"Bad Gateway">>;
format_response_code(503) -> <<"Service Unavailable">>;
format_response_code(504) -> <<"Server Time-out">>;
format_response_code(505) -> <<"Version Not Supported">>;
format_response_code(513) -> <<"Message Too Large">>;
format_response_code(580) -> <<"Precondition Failure">>;

format_response_code(600) -> <<"Busy Everywhere">>;
format_response_code(603) -> <<"Decline">>;
format_response_code(604) -> <<"Does Not Exist Anywhere">>;
format_response_code(606) -> <<"Not Acceptable">>.

-spec decode/1 :: (iolist() | binary()) -> binary().
decode(L) when is_list(L) ->
    decode(iolist_to_binary(L));
decode(B) ->
    wh_util:to_lower_binary(cowboy_http:urldecode(B)).

-spec decode_uri/1 :: (binary()) -> binary().
decode_uri(B) when is_binary(B) ->
    decode_uri(B, []).
decode_uri(<<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
decode_uri(<<$%, H, L, B/binary>>, Acc) ->
    decode_uri(B, [ (unhex(H) bsl 4 bor unhex(L)) | Acc]);
decode_uri(<<C:1/binary, B/binary>>, Acc) ->
    decode_uri(B, [C | Acc]).

-spec unhex(byte()) -> byte() | error.
unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(_) -> exit(badarg).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

sip_uri_full_test() ->
    Uri = <<"sip:alice:pass@atlanta.com;method=INVITE;maddr=239.255.255.1;ttl=15?day=tuesday">>,
    {#sip_uri{scheme=Scheme
              ,user=U
              ,password=P
              ,host=H
              ,port=Port
              ,params=Params
              ,headers=Hdrs
             }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"alice">>, U),
    ?assertEqual(<<"pass">>, P),
    ?assertEqual(<<"atlanta.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([{<<"day">>, <<"tuesday">>}], Hdrs),

    #sip_uri_params{transport=Tr
                    ,maddr=Ma
                    ,ttl=TTL
                    ,user=User
                    ,method=Me
                    ,lr=LR
                    ,other=O
                   } = Params,

    ?assertEqual(undefined, Tr),
    ?assertEqual(<<"239.255.255.1">>, Ma),
    ?assertEqual(15, TTL),
    ?assertEqual('undefined', User),
    ?assertEqual('INVITE', Me),
    ?assertEqual('undefined', LR),
    ?assertEqual([], O).

sip_uri_host_only_test() ->
    Uri = <<"sip:atlanta.com;method=REGISTER?to=alice%40atlanta.com">>,
    {#sip_uri{scheme=Scheme
              ,user=U
              ,password=P
              ,host=H
              ,port=Port
              ,params=Params
              ,headers=Hdrs
             }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('sip', Scheme),
    ?assertEqual('undefined', U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"atlanta.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([{<<"to">>, <<"alice@atlanta.com">>}], Hdrs),

    #sip_uri_params{transport=Tr
                    ,maddr=Ma
                    ,ttl=TTL
                    ,user=User
                    ,method=Me
                    ,lr=LR
                    ,other=O
                   } = Params,

    ?assertEqual('undefined', Tr),
    ?assertEqual('undefined', Ma),
    ?assertEqual(undefined, TTL),
    ?assertEqual('undefined', User),
    ?assertEqual('REGISTER', Me),
    ?assertEqual('undefined', LR),
    ?assertEqual([], O).

sip_uri_weird_username_test() ->
    Uri = <<"sips:alice;day=tuesday@atlanta.com">>,
    {#sip_uri{scheme=Scheme
              ,user=U
              ,password=P
              ,host=H
              ,port=Port
              ,params=Params
              ,headers=Hdrs
             }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('sips', Scheme),
    ?assertEqual(<<"alice;day=tuesday">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"atlanta.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),

    #sip_uri_params{transport=Tr
                    ,maddr=Ma
                    ,ttl=TTL
                    ,user=User
                    ,method=Me
                    ,lr=LR
                    ,other=O
                   } = Params,

    ?assertEqual('undefined', Tr),
    ?assertEqual('undefined', Ma),
    ?assertEqual('undefined', TTL),
    ?assertEqual('undefined', User),
    ?assertEqual('undefined', Me),
    ?assertEqual('undefined', LR),
    ?assertEqual([], O).

sip_uri_user_and_ip_test() ->
    Uri = <<"sip:alice@192.168.1.1">>,
    {#sip_uri{scheme=Scheme
              ,user=U
              ,password=P
              ,host=H
              ,port=Port
              ,params=Params
              ,headers=Hdrs
             }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"alice">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"192.168.1.1">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),

    #sip_uri_params{transport=Tr
                    ,maddr=Ma
                    ,ttl=TTL
                    ,user=User
                    ,method=Me
                    ,lr=LR
                    ,other=O
                   } = Params,

    ?assertEqual('undefined', Tr),
    ?assertEqual('undefined', Ma),
    ?assertEqual('undefined', TTL),
    ?assertEqual('undefined', User),
    ?assertEqual('undefined', Me),
    ?assertEqual('undefined', LR),
    ?assertEqual([], O).

sip_uri_did_user_test() ->
    Uri = <<"sip:+1-212-555-1212:1234@gateway.com;user=phone">>,
    {#sip_uri{scheme=Scheme
              ,user=U
              ,password=P
              ,host=H
              ,port=Port
              ,params=Params
              ,headers=Hdrs
             }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"+1-212-555-1212">>, U),
    ?assertEqual(<<"1234">>, P),
    ?assertEqual(<<"gateway.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),

    #sip_uri_params{transport=Tr
                    ,maddr=Ma
                    ,ttl=TTL
                    ,user=User
                    ,method=Me
                    ,lr=LR
                    ,other=O
                   } = Params,

    ?assertEqual('undefined', Tr),
    ?assertEqual('undefined', Ma),
    ?assertEqual('undefined', TTL),
    ?assertEqual(<<"phone">>, User),
    ?assertEqual('undefined', Me),
    ?assertEqual('undefined', LR),
    ?assertEqual([], O).

sip_uri_encoded_user_test() ->
    Uri = <<"sip:%61lice@atlanta.com;transport=TCP">>,
    {#sip_uri{scheme=Scheme
              ,user=U
              ,password=P
              ,host=H
              ,port=Port
              ,params=Params
              ,headers=Hdrs
             }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"alice">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"atlanta.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),

    #sip_uri_params{transport=Tr
                    ,maddr=Ma
                    ,ttl=TTL
                    ,user=User
                    ,method=Me
                    ,lr=LR
                    ,other=O
                   } = Params,

    ?assertEqual('tcp', Tr),
    ?assertEqual('undefined', Ma),
    ?assertEqual('undefined', TTL),
    ?assertEqual('undefined', User),
    ?assertEqual('undefined', Me),
    ?assertEqual('undefined', LR),
    ?assertEqual([], O).

sip_uri_other_params_test() ->
    Uri = <<"sip:carol@chicago.com;newparam=5;method=REGISTER">>,
    {#sip_uri{scheme=Scheme
              ,user=U
              ,password=P
              ,host=H
              ,port=Port
              ,params=Params
              ,headers=Hdrs
             }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"carol">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"chicago.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),

    #sip_uri_params{transport=Tr
                    ,maddr=Ma
                    ,ttl=TTL
                    ,user=User
                    ,method=Me
                    ,lr=LR
                    ,other=O
                   } = Params,

    ?assertEqual('undefined', Tr),
    ?assertEqual('undefined', Ma),
    ?assertEqual('undefined', TTL),
    ?assertEqual('undefined', User),
    ?assertEqual('REGISTER', Me),
    ?assertEqual('undefined', LR),
    ?assertEqual([{<<"newparam">>, <<"5">>}], O).
-endif.
