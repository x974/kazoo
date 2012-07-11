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
         ,is_known_header/1

         ,format_response_code/1
        ]).

-include("smoke.hrl").
-include("smoke_sip.hrl").
-include_lib("eunit/include/eunit.hrl").

-export_type([sip_method/0
              ,sip_version/0
              ,sip_header/0
              ,sip_headers/0
              ,sip_status/0
              ,sip_transport/0
             ]).

-spec version/0 :: () -> sip_version().
version() -> ?SIP_VERSION_2_0.

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
is_known_transport(T) when is_binary(T) ->
    case catch is_known_transport(wh_util:to_atom(wh_util:to_lower_binary(T))) of
        {'EXIT', _} -> false;
        {true, _}=OK -> OK
    end;
is_known_transport(T) when is_list(T) ->
    case catch is_known_transport(wh_util:to_atom(wh_util:to_lower(T))) of
        {'EXIT', _} -> false;
        {true, _}=OK -> OK
    end;
is_known_transport(T) when is_atom(T) ->
    case lists:member(T, ?TRANSPORTS_SUPPORTED) of
        true -> {true, T};
        false -> false
    end.

-spec is_known_header/1 :: (atom() | string() | binary()) -> 'false' | {'true', sip_header()}.
is_known_header(H) ->
    case catch wh_util:to_atom(H) of
        {'EXIT', _} -> false;
        Hatom when is_atom(Hatom) ->
            case lists:member(Hatom, ?HEADERS_SUPPORTED) of
                true -> {true, Hatom};
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
parse_sip_packet(Buffer, M, #sip_uri{}=RUri, _Vsn) ->
    {SipHeaders, Body} = parse_sip_headers(Buffer),
    Req = lists:foldl(fun({F, D}, R) -> smoke_sip_req:F(R, D) end
                      ,smoke_sip_req:new()
                      ,[{set_method, M}
                        ,{set_request_uri, RUri}
                        ,{set_headers, SipHeaders}
                       ]),
    {ok, smoke_sip_req:set_request_body(Req, Body)}.

%% Parse out the headers and return the list of setters with values, as well as
%% the body (if any)
-spec parse_sip_headers/1 :: (binary()) ->
                                     {[{sip_header() | ne_binary(), ne_binary() | integer()}], binary()} |
                                     {'error', sip_response_code()}.
parse_sip_headers(Buffer) ->
    parse_sip_headers(Buffer, [], []).

%% Buffer, ListOfHeadersParsed, HeaderKeySoFar
parse_sip_headers(<<"\r\n\r\n", Buffer/binary>>, Headers, _) ->
    {Headers, Buffer};
parse_sip_headers(<<>>, Headers, _) ->
    {Headers, <<>>};
parse_sip_headers(<<" ", Buffer/binary>>, Headers, HeaderKey) ->
    %% ignore whitespace between Key and :
    parse_sip_headers(extract_while(Buffer, ws), Headers, HeaderKey);
parse_sip_headers(<<":", Buffer/binary>>, Headers, HeaderKey) ->
    case parse_sip_header_value(extract_while(Buffer, ws), list_to_binary(lists:reverse(HeaderKey))) of
        {error, _}=E -> E;
        {Buffer1, K, V} -> parse_sip_headers(Buffer1, [{K,V} | Headers], [])
    end;
parse_sip_headers(<<"\r\n", Buffer/binary>>, Headers, _) ->
    parse_sip_headers(Buffer, Headers, []);
parse_sip_headers(<<K, Buffer/binary>>, Headers, HeaderKey) ->
    parse_sip_headers(Buffer, Headers, [K | HeaderKey]).

-spec parse_sip_header_value/2 :: (ne_binary(), ne_binary()) -> {ne_binary(), sip_header() | ne_binary(), ne_binary()}.
parse_sip_header_value(<<" ", Buffer/binary>>, Key) ->
    %% remove whitespace after the :
    parse_sip_header_value(Buffer, Key);
parse_sip_header_value(Buffer, Key) ->
    case is_known_header(Key) of
        {true, Header} -> parse_sip_header_value(Buffer, Header, []);
        false -> parse_sip_header_value(Buffer, Key, [])
    end.

parse_sip_header_value(<<"\r\n\r\n", _/binary>> = Buffer, Key, Acc) ->
    %% otherwise, newline indicates end of value collection
    V = list_to_binary(lists:reverse(Acc)),
    {Buffer, Key, format_sip_header_value(Key, V)};
parse_sip_header_value(<<"\r\n ", Buffer/binary>>, Key, Acc) ->
    %% newline with a space - continued value
    parse_sip_header_value(Buffer, Key, Acc);
parse_sip_header_value(<<"\r\n\t", Buffer/binary>>, Key, Acc) ->
    %% newline with a horizontal tab - continued value
    parse_sip_header_value(Buffer, Key, Acc);
parse_sip_header_value(<<"\r\n", Buffer/binary>>, Key, Acc) ->
    %% otherwise, newline indicates end of value collection
    V = list_to_binary(lists:reverse(Acc)),
    {Buffer, Key, format_sip_header_value(Key, V)};
parse_sip_header_value(<<"\n", Buffer/binary>>, Key, Acc) ->
    %% otherwise, newline indicates end of value collection
    V = list_to_binary(lists:reverse(Acc)),
    {Buffer, Key, format_sip_header_value(Key, V)};
parse_sip_header_value(<<V, Buffer/binary>>, Key, Acc) ->
    parse_sip_header_value(Buffer, Key, [V | Acc]).

extract_sip_version(<<"SIP/2.0", Buffer/binary>>) ->
    {version(), Buffer};
extract_sip_version(Buffer) ->
    case extract_until(Buffer, $ ) of
        {terminator, _, Buffer1} -> extract_sip_version(Buffer1);
        _ -> {error, 505}
    end.

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
extract_sip_uri(Buffer) ->
    case extract_until(Buffer, $ ) of
        {terminator, Acc, Buffer1} ->
            % Could be 'Name <sip:user@realm>' or '"Name" <sip:user@realm>'
            case extract_sip_uri_1(Acc) of
                {error, _} ->
                    case extract_sip_uri_1(Buffer1) of
                        {error, _}=E -> E;
                        {#sip_uri{}=Uri, Buffer2} ->
                            {Uri#sip_uri{display_name=Acc}, Buffer2}
                    end;
                {#sip_uri{}=Uri, <<>>} ->
                    {Uri, Buffer1};
                {#sip_uri{}=Uri, Buffer2} ->
                    {Uri, <<Buffer2/binary, Buffer1/binary>>}
            end;
        {eol, Line, Buffer1} ->
            case extract_sip_uri_1(Line) of
                {error, _}=E -> E;
                {#sip_uri{}=Uri, _} -> {Uri, Buffer1}
            end;
        {eof, Line, <<>>} ->
            extract_sip_uri_1(Line)
    end.
    
extract_sip_uri_1(<<"sip:", Buffer/binary>>) ->
    extract_sip_uri(Buffer, 'sip');
extract_sip_uri_1(<<"sips:", Buffer/binary>>) ->
    extract_sip_uri(Buffer, 'sips');
extract_sip_uri_1(<<"<sip:", Buffer/binary>>) ->
    extract_sip_uri(Buffer, 'sip');             
extract_sip_uri_1(<<"<sips:", Buffer/binary>>) ->
    extract_sip_uri(Buffer, 'sips');
extract_sip_uri_1(_B) ->
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
extract_sip_host(<<";", _/binary>> = Buffer, Acc) ->
    % no port, at host params boundry
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_host(<<" ", _/binary>> = Buffer, Acc) ->
    % no port, at host params boundry
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_host(<<">", Buffer/binary>>, Acc) ->
    % end of the host, at possible params boundry
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_host(<<"\r\n", _/binary>> = Buffer, Acc) ->
    % no port, at line ending
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_host(<<"\n", _/binary>> = Buffer, Acc) ->
    % no port, at line ending
    {iolist_to_binary(lists:reverse(Acc)), undefined, Buffer};
extract_sip_host(<<H:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_host(Buffer, [H | Acc]).

-spec extract_sip_port/1 :: (ne_binary()) -> {pos_integer(), ne_binary()}.
-spec extract_sip_port/2 :: (ne_binary(), list()) -> {pos_integer(), ne_binary()}.
extract_sip_port(Buffer) ->
    extract_sip_port(Buffer, []).

extract_sip_port(<<";", _/binary>> = Buffer, Acc) ->
    % at params boundry
    {wh_util:to_integer([D || <<D>> <- lists:reverse(Acc)]), Buffer};
extract_sip_port(<<" ", _/binary>> = Buffer, Acc) ->
    % at end of URI
    {wh_util:to_integer([D || <<D>> <- lists:reverse(Acc)]), Buffer};
extract_sip_port(<<">", Buffer/binary>>, Acc) ->
    % at end of host:port, could be params/headers afterwards
    {wh_util:to_integer([D || <<D>> <- lists:reverse(Acc)]), Buffer};
extract_sip_port(<<"\r\n", _/binary>> = Buffer, Acc) ->
    % no port, at line ending
    {wh_util:to_integer([D || <<D>> <- lists:reverse(Acc)]), Buffer};
extract_sip_port(<<"\n", _/binary>> = Buffer, Acc) ->
    % no port, at line ending
    {wh_util:to_integer([D || <<D>> <- lists:reverse(Acc)]), Buffer};
extract_sip_port(<<P:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_port(Buffer, [P | Acc]).

-spec extract_sip_params_headers/1 :: (ne_binary()) -> {ne_binary(), binary(), ne_binary()}.
extract_sip_params_headers(Buffer) ->
    extract_sip_params(Buffer, []).

extract_sip_params(<<>>, Params) ->
    {Params, [], <<>>};
extract_sip_params(<<"?", Buffer/binary>>, Params) ->
    % at headers boundry
    {Hdrs, Buffer1} = extract_sip_headers(Buffer),
    {Params, Hdrs, Buffer1};
extract_sip_params(<<" ", _/binary>> = Buffer, Params) ->
    % no headers, at line ending
    {Params, [], Buffer};

extract_sip_params(<<"\r\n ", Buffer/binary>>, Params) ->
    % line ending with a space, params continue
    extract_sip_params(Buffer, Params);
extract_sip_params(<<"\r\n\t", Buffer/binary>>, Params) ->
    % line ending with a horiz-tab, params continue
    extract_sip_params(Buffer, Params);
extract_sip_params(<<"\r\n", Buffer/binary>>, Params) ->
    % no headers, at line ending
    {Params, [], Buffer};

extract_sip_params(<<"\n ", Buffer/binary>>, Params) ->
    % line ending with a space, params continue
    extract_sip_params(Buffer, Params);
extract_sip_params(<<"\n\t", Buffer/binary>>, Params) ->
    % line ending with a horiz-tab, params continue
    extract_sip_params(Buffer, Params);
extract_sip_params(<<"\n", _/binary>> = Buffer, Params) ->
    % no headers, at line ending
    {Params, [], Buffer};
extract_sip_params(<<">", Buffer/binary>>, Params) ->
    % end of the URI sometimes
    extract_sip_params(extract_while(extract_while(Buffer, lws), ws)
                       ,Params
                      );
extract_sip_params(Buffer, Params) ->
    {Key, Buffer1} = extract_sip_param_key(Buffer),
    case props:get_value(Key, Params) of
        undefined ->
            {V, Buffer2} = extract_sip_param_value(Buffer1),
            case V of
                <<>> -> extract_sip_params(extract_while(extract_while(Buffer2, lws), ws)
                                           ,[{Key, true}|Params]
                                          );
                _ -> extract_sip_params(extract_while(extract_while(Buffer2, lws), ws)
                                        ,[{Key, format_sip_uri_param(Key, V)}
                                          |Params
                                         ]
                                       )
            end;
        _V ->
            %% key has been defined, error!
            {error, 400}
    end.

-spec extract_sip_param_key/1 :: (ne_binary()) -> {ne_binary(), ne_binary()}.
extract_sip_param_key(Buffer) ->
    extract_sip_param_key(Buffer, []).
extract_sip_param_key(<<"=", Buffer/binary>>, Acc) ->
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_key(<<";", Buffer/binary>>, []) ->
    %% start of a key
    extract_sip_param_key(Buffer, []);
extract_sip_param_key(<<";", _/binary>> = Buffer, Acc) ->
    %% key with no value
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_key(<<">", _/binary>> = Buffer, Acc) ->
    %% end of a key with no value and end of params
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_key(<<"?", _/binary>> = Buffer, Acc) ->
    %% end of a key with no value and end of params, start of headers
    {decode(lists:reverse(Acc)), Buffer};
extract_sip_param_key(<<K:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_param_key(Buffer, [K | Acc]).

extract_sip_param_value(Buffer) ->
    extract_sip_param_value(Buffer, []).
extract_sip_param_value(<<";", _/binary>> = Buffer, Acc) ->
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
extract_sip_param_value(<<">", _/binary>> = Buffer, []) ->
    {true, Buffer};
extract_sip_param_value(<<">", _/binary>> = Buffer, Acc) ->
    % end of value
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
-spec extract_until/2 :: (binary(), byte() | ne_binary() | 'undefined') ->
                                 {'terminator' | 'eol' | 'eof', binary(), binary()}.
-spec extract_until/3 :: (binary()
                          ,byte() | ne_binary() | 'undefined'
                          ,'undefined' | 'ignore_eol' | 'end_of_headers'
                         ) ->
                                 {'terminator' | 'eol' | 'eof', binary(), binary()}.
extract_until(Buffer, Terminator) ->
    extract_until(Buffer, Terminator, undefined, []).
extract_until(Buffer, Terminator, Opt) ->
    extract_until(Buffer, Terminator, Opt, []).

extract_until(<<"\r\n\r\n", Buffer/binary>>, _, end_of_headers, Acc) ->
    {terminator, list_to_binary(lists:reverse(Acc)), Buffer};
extract_until(<<C, Buffer/binary>>, T, end_of_headers, Acc) ->
    extract_until(Buffer, T, end_of_headers, [C | Acc]);

extract_until(<<Terminator, Buffer/binary>>, Terminator, _, Acc) ->
    {terminator, list_to_binary(lists:reverse(Acc)), Buffer};
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

%% pop off Terminator until a non-terminator character is encountered
-spec extract_while/2 :: (binary(), char() | 'lws' | 'hcolon' | 'ws') -> binary().
extract_while(<<"\r\n\t", Buffer/binary>>, lws) -> extract_while(Buffer, lws);
extract_while(<<"\r\n ", Buffer/binary>>, lws) -> extract_while(Buffer, lws);

extract_while(<<$\t, Buffer/binary>>, ws) -> extract_while(Buffer, ws);
extract_while(<<$ , Buffer/binary>>, ws) -> extract_while(Buffer, ws);

extract_while(<<"\r\n", Buffer/binary>>, hcolon) -> extract_while(Buffer, hcolon);
extract_while(<<" ", Buffer/binary>>, hcolon) -> extract_while(Buffer, hcolon);

extract_while(<<Terminator, Buffer/binary>>, Terminator) ->
    extract_while(Buffer, Terminator);
extract_while(Buffer, _) ->
    Buffer.

-spec format_sip_header_value/2 :: (sip_header() | ne_binary(), ne_binary()) -> ne_binary() |
                                                                                sip_uri() |
                                                                                integer() |
                                                                                {integer(), sip_method()}.
format_sip_header_value('To', V) ->
    case extract_sip_uri(V) of
        {error, _}=E -> throw(E);
        {Uri, <<>>} -> Uri
    end;
format_sip_header_value('From', V) ->
    case extract_sip_uri(V) of
        {error, _}=E -> throw(E);
        {Uri, <<>>} -> Uri
    end;
format_sip_header_value('Contact', V) ->
    case extract_sip_uri(V) of
        {error, _}=E -> throw(E);
        {Uri, <<>>} -> Uri
    end;
format_sip_header_value('Reply-To', V) ->
    case extract_sip_uri(V) of
        {error, _}=E -> throw(E);
        {Uri, <<>>} -> Uri
    end;

format_sip_header_value('Max-Forwards', V) ->
    case wh_util:to_integer(V) of
        N when N =< 0 -> throw({error, 483}); % too many hops
        N when N > 255 -> throw({error, 400}); % invalid range
        N -> N
    end;

format_sip_header_value('CSeq', V) ->
    case extract_until(V, $ ) of
        {terminator, Seq, M} ->
            SeqN = case wh_util:to_integer(Seq) of N when N < 2147483648 -> N end, % Seq must be < 2^31
            {true, Method} = is_known_method(M),
            {SeqN, Method}
    end;

format_sip_header_value('Via', V) ->
    format_via(V);
format_sip_header_value('Content-Length', V) ->
    case wh_util:to_integer(V) of
        N when N >= 0 -> N;
        _ -> throw({error, 400})
    end;

format_sip_header_value('Allow', V) -> format_allow_methods(V);

format_sip_header_value('Content-Type', V) ->
    {_,_,_} = cowboy_http:content_type(V);
format_sip_header_value('Accept', V) ->
    {_,_,_} = cowboy_http:content_type(V);

format_sip_header_value('Accept-Encoding', V) ->
    case cowboy_http:list(V, fun cowboy_http:conneg/2) of
        {error, badarg} -> throw({error, 400});
        L when is_list(L) -> L
    end;
format_sip_header_value('Content-Encoding', V) ->
    case cowboy_http:list(V, fun cowboy_http:conneg/2) of
        {error, badarg} -> throw({error, 400});
        L when is_list(L) -> L
    end;

format_sip_header_value('Accept-Language', V) ->
    case cowboy_http:nonempty_list(V, fun cowboy_http:language_range/2) of
        {error, badarg} -> throw({error, 400});
        L when is_list(L) -> L
    end;
format_sip_header_value('Content-Language', V) ->
    case cowboy_http:nonempty_list(V, fun cowboy_http:language_range/2) of
        {error, badarg} -> throw({error, 400});
        L when is_list(L) -> L
    end;

format_sip_header_value('Date', V) ->
    case cowboy_http:rfc1123_date(V) of
        {error, badarg} -> throw({error, 400});
        D -> D
    end;

format_sip_header_value('Expires', V) ->
    case wh_util:to_integer(V) of
        N when N >= 0 andalso N < 4294967295 -> N; % 0 <= N < 2^32 - 1
        _ -> throw({error, 400})
    end;

format_sip_header_value('In-Reply-To', V) ->
    format_in_reply_to(V);

format_sip_header_value('Min-Expires', V) ->
    case wh_util:to_integer(V) of
        N when N >= 0 andalso N < 4294967295 -> N; % 0 <= N < 2^32 - 1
        _ -> throw({error, 400})
    end;

format_sip_header_value('Retry-After', V) ->
    format_retry_after(V);

format_sip_header_value('Timestamp', V) ->
    wh_util:to_integer(V);

format_sip_header_value(_, V) -> V.
    %%                         %% Optional Headers
    %%                         'Alert-Info', 'Authentication-Info',
    %%                         'Authorization', 'Call-Info', 'Content-Disposition',
    %%                         'Error-Info'
    %%                         'MIME-Version'
    %%                         'Proxy-Authenticate', 'Proxy-Authorization', 'Proxy-Require',
    %%                         'Record-Route', 'Require'
    %%                         'Route', 'Supported'
    %%                         'Unsupported', 'Warning', 'WWW-Authenticate') -> 

-spec format_sip_uri_param/2 :: (ne_binary(), ne_binary() | 'true') -> ne_binary() | integer() |
                                                                       sip_method() | sip_transport().
format_sip_uri_param(<<"transport">>, V) ->
    case is_known_transport(V) of
        {true, T} -> T;
        false -> throw({error, 400})
    end;
format_sip_uri_param(<<"method">>, V) ->
    case is_known_method(V) of
        {true, M} -> M;
        false -> throw({error, 400})
    end;
format_sip_uri_param(<<"maddr">>, V) ->
    wh_util:to_lower_binary(V);
format_sip_uri_param(<<"ttl">>, V) ->
    wh_util:to_integer(V);
format_sip_uri_param(<<"user">>, V) ->
    wh_util:to_lower_binary(V);
format_sip_uri_param(_K, V) -> V.

-spec format_via/1 :: (ne_binary()) -> sip_via().
format_via(<<"SIP", Buffer/binary>>) ->
    {terminator, _T, Buffer1} = extract_until(Buffer, $/),
    <<"2.0", Buffer2/binary>> = extract_while(Buffer1, $ ),
    {terminator, _, Buffer3} = extract_until(Buffer2, $/),
    {Via, _} = extract_via_transport(extract_while(Buffer3, $ )),
    Via.

extract_via_transport(Buffer) ->
    case extract_until(Buffer, $ ) of
        {terminator, T, Buffer1} ->
            case is_known_transport(T) of
                {true, Transport} ->
                    {H, Buffer2} = extract_via_host(Buffer1),
                    {P, Buffer3} = extract_via_port(Buffer2),
                    {Params, _Hdrs, Buffer4} = extract_sip_params_headers(Buffer3),
                    {#sip_via{transport=Transport
                              ,host=H
                              ,port=P
                              ,params=Params
                             }
                     ,Buffer4};
                false ->
                    throw({error, 400})
            end;
        _ -> throw({error, 400})
    end.

-spec extract_via_host/1 :: (ne_binary()) -> {ne_binary(), binary()}.
extract_via_host(Buffer) ->
    extract_via_host(extract_while(Buffer, $ ), []).

extract_via_host(<<":", Buffer/binary>>, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Buffer};
extract_via_host(<<";", _/binary>> = Buffer, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Buffer};
extract_via_host(<<" ", Buffer/binary>>, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Buffer};
extract_via_host(<<>>, Acc) ->
    {list_to_binary(lists:reverse(Acc)), <<>>};
extract_via_host(<<H, Buffer/binary>>, Acc) ->
    extract_via_host(Buffer, [H | Acc]).

-spec extract_via_port/1 :: (binary()) -> {'undefined' | integer(), binary()}.
extract_via_port(<<>>) -> {undefined, <<>>};
extract_via_port(Buffer) ->
    extract_via_port(extract_while(Buffer, $ ), []).

extract_via_port(<<";", _/binary>> = Buffer, []) ->
    {undefined, Buffer};
extract_via_port(<<";", _/binary>> = Buffer, Acc) ->
    {wh_util:to_integer(lists:reverse(Acc)), Buffer};
extract_via_port(<<":", Buffer/binary>>, []) ->
    %% okay, definitely starting a port number
    extract_via_port(extract_while(Buffer, $ ), []);
extract_via_port(<<>>, []) ->
    {undefined, <<>>};
extract_via_port(<<>>, Acc) ->
    {wh_util:to_integer(lists:reverse(Acc)), <<>>};
extract_via_port(<<P, Buffer/binary>>, Acc) ->
    extract_via_port(Buffer, [P | Acc]).

-spec format_allow_methods/1 :: (ne_binary()) -> [sip_method()].
format_allow_methods(Buffer) ->
    format_allow_methods(Buffer, [], []).
format_allow_methods(<<",", Buffer/binary>>, Methods, Acc) ->
    case is_known_method(list_to_binary(lists:reverse(Acc))) of
        false -> format_allow_methods(Buffer, Methods, []);
        {true, M} -> format_allow_methods(Buffer, [M | Methods], [])
    end;
format_allow_methods(<<" ", Buffer/binary>>, Methods, Acc) ->
    format_allow_methods(Buffer, Methods, Acc);
format_allow_methods(<<M, Buffer/binary>>, Methods, Acc) ->
    format_allow_methods(Buffer, Methods, [M | Acc]);
format_allow_methods(<<>>, Methods, []) ->
    Methods;
format_allow_methods(<<>>, Methods, Acc) ->
    case is_known_method(list_to_binary(lists:reverse(Acc))) of
        false -> Methods;
        {true, M} -> [M | Methods]
    end.

-spec format_in_reply_to/1 :: (ne_binary()) -> [ne_binary()].
format_in_reply_to(Buffer) ->
    format_in_reply_to(Buffer, [], []).
format_in_reply_to(<<",", Buffer/binary>>, CallIds, Acc) ->
    format_in_reply_to(Buffer, [list_to_binary(lists:reverse(Acc)) | CallIds], []);
format_in_reply_to(<<" ", Buffer/binary>>, CallIds, Acc) ->
    format_in_reply_to(Buffer, CallIds, Acc);
format_in_reply_to(<<C, Buffer/binary>>, CallIds, Acc) ->
    format_in_reply_to(Buffer, CallIds, [C | Acc]);
format_in_reply_to(<<>>, CallIds, []) ->
    CallIds;
format_in_reply_to(<<>>, CallIds, Acc) ->
    [list_to_binary(lists:reverse(Acc)) | CallIds].

-spec format_retry_after/1 :: (ne_binary()) ->
                                      {integer(), integer() | 'undefined', ne_binary() | 'undefined'}.
format_retry_after(Buffer) ->
    format_retry_after(Buffer, []).
format_retry_after(<<";duration=", Buffer/binary>>, Digits) ->
    Retry = wh_util:to_integer(lists:reverse(Digits)),
    {Dur, Buffer1} = extract_retry_duration(Buffer),
    {Retry, Dur, Buffer1};
format_retry_after(<<" ", Buffer/binary>>, Digits) ->
    Retry = wh_util:to_integer(lists:reverse(Digits)),
    {Retry, undefined, Buffer};
format_retry_after(<<>>, Digits) ->
    {wh_util:to_integer(lists:reverse(Digits)), undefined, undefined};
format_retry_after(<<D, Buffer/binary>>, Digits) ->
    format_retry_after(Buffer, [D | Digits]).

extract_retry_duration(Buffer) ->
    extract_retry_duration(Buffer, []).
extract_retry_duration(<<" ", Buffer/binary>>, Acc) ->
    {wh_util:to_integer(lists:reverse(Acc)), Buffer};
extract_retry_duration(<<>>, Acc) ->
    {wh_util:to_integer(lists:reverse(Acc)), <<>>};
extract_retry_duration(<<D, Buffer/binary>>, Acc) ->
    extract_retry_duration(Buffer, [D | Acc]).

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

-ifdef(TEST).

sip_uri_full_test() ->
    Uri = <<"sip:alice:pass@atlanta.com;method=INVITE;maddr=239.255.255.1;ttl=15?day=tuesday">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('undefined', DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"alice">>, U),
    ?assertEqual(<<"pass">>, P),
    ?assertEqual(<<"atlanta.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([{<<"day">>, <<"tuesday">>}], Hdrs),

    ?assertEqual('undefined', props:get_value(<<"transport">>, Params)),
    ?assertEqual(<<"239.255.255.1">>, props:get_value(<<"maddr">>, Params)),
    ?assertEqual(15, props:get_value(<<"ttl">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"user">>, Params)),
    ?assertEqual('INVITE', props:get_value(<<"method">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"lr">>, Params)).

sip_uri_host_only_test() ->
    Uri = <<"sip:atlanta.com;method=REGISTER?to=alice%40atlanta.com">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('undefined', DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual('undefined', U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"atlanta.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([{<<"to">>, <<"alice@atlanta.com">>}], Hdrs),

    ?assertEqual('undefined', props:get_value(<<"transport">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"maddr">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"ttl">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"user">>, Params)),
    ?assertEqual('REGISTER', props:get_value(<<"method">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"lr">>, Params)).

sip_uri_weird_username_test() ->
    Uri = <<"sips:alice;day=tuesday@atlanta.com">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('undefined', DN),
    ?assertEqual('sips', Scheme),
    ?assertEqual(<<"alice;day=tuesday">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"atlanta.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),
    ?assertEqual([], Params).

sip_uri_user_and_ip_test() ->
    Uri = <<"sip:alice@192.168.1.1 ignore">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,B} = extract_sip_uri(Uri),

    ?assertEqual(<<"ignore">>, B),

    ?assertEqual('undefined', DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"alice">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"192.168.1.1">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),
    ?assertEqual([], Params).

sip_uri_did_user_test() ->
    Uri = <<"sip:+1-212-555-1212:1234@gateway.com;user=phone">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('undefined', DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"+1-212-555-1212">>, U),
    ?assertEqual(<<"1234">>, P),
    ?assertEqual(<<"gateway.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),
    ?assertEqual(<<"phone">>, props:get_value(<<"user">>, Params)).

sip_uri_encoded_user_test() ->
    Uri = <<"sip:%61lice@atlanta.com;transport=TCP">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('undefined', DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"alice">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"atlanta.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),

    ?assertEqual('tcp', props:get_value(<<"transport">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"maddr">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"ttl">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"user">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"method">>, Params)),
    ?assertEqual('undefined', props:get_value(<<"lr">>, Params)).

sip_uri_other_params_test() ->
    Uri = <<"sip:carol@chicago.com;newparam=5;method=REGISTER">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('undefined', DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"carol">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"chicago.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),

    ?assertEqual('REGISTER', props:get_value(<<"method">>, Params)),
    ?assertEqual(<<"5">>, props:get_value(<<"newparam">>, Params)).

sip_uri_display_name_test() ->
    Uri = <<"Carol <sip:carol@chicago.com>">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual(<<"Carol">>, DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"carol">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"chicago.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),
    ?assertEqual([], Params).

sip_uri_display_name_quoted_test() ->
    Uri = <<"\"Carol\" <sip:carol@chicago.com>">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual(<<"\"Carol\"">>, DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"carol">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"chicago.com">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),
    ?assertEqual([], Params).

sip_uri_display_name_tags_test() ->
    Uri = <<"\"\" <sip:0000000000@192.168.1.1>;rport;tag=3et3X2avH64Xr">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual(<<"\"\"">>, DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"0000000000">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"192.168.1.1">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),

    ?assertEqual(<<"3et3x2avh64xr">>, props:get_value(<<"tag">>, Params)),
    ?assertEqual('true', props:get_value(<<"rport">>, Params)).

sip_uri_lr_param_test() ->
    Uri = <<"<sip:alice@192.168.1.1;lr>">>,
    {#sip_uri{
        display_name=DN
        ,scheme=Scheme
        ,user=U
        ,password=P
        ,host=H
        ,port=Port
        ,params=Params
        ,headers=Hdrs
       }
     ,_} = extract_sip_uri(Uri),

    ?assertEqual('undefined', DN),
    ?assertEqual('sip', Scheme),
    ?assertEqual(<<"alice">>, U),
    ?assertEqual('undefined', P),
    ?assertEqual(<<"192.168.1.1">>, H),
    ?assertEqual('undefined', Port),
    ?assertEqual([], Hdrs),
    ?assertEqual(true, props:get_value(<<"lr">>, Params)).

via_value_test() ->
    Via = <<"SIP  /  2.0  /  UDP first.example.com: 4000;ttl=16\r\n\t;maddr=224.2.0.1 ;branch=z9hG4bKa7c6a8dlze.1">>,
    #sip_via{version=Ver
             ,transport=TP
             ,host=H
             ,port=P
             ,params = Params
            } = format_sip_header_value('Via', Via),
    ?assertEqual(?SIP_VERSION_2_0, Ver),
    ?assertEqual('udp', TP),
    ?assertEqual(<<"first.example.com">>, H),
    ?assertEqual(4000, P),

    ?assertEqual(16, props:get_value(<<"ttl">>, Params)),
    ?assertEqual(<<"224.2.0.1">>, props:get_value(<<"maddr">>, Params)),
    ?assertEqual(<<"z9hg4bka7c6a8dlze.1">>, props:get_value(<<"branch">>, Params)).

via_header_test() ->
    Via = <<"SIP/2.0/UDP 192.168.1.1;rport;branch=z9hG4bK17DUN85HDeFyQ">>,
    #sip_via{version=Ver
             ,transport=TP
             ,host=H
             ,port=P
             ,params = Params
            } = format_sip_header_value('Via', Via),
    ?assertEqual(?SIP_VERSION_2_0, Ver),
    ?assertEqual('udp', TP),
    ?assertEqual(<<"192.168.1.1">>, H),
    ?assertEqual('undefined', P),

    ?assertEqual(<<"z9hg4bk17dun85hdefyq">>, props:get_value(<<"branch">>, Params)),
    ?assertEqual(true, props:get_value(<<"rport">>, Params)).

sip_packet_invite_test() ->
    SIP = <<"INVITE sip:alice@192.168.1.176 SIP/2.0\r\nVia: SIP/2.0/UDP 192.168.1.45;rport;branch=z9hG4bK2mBSta3rrB99D\r\nMax-Forwards: 70\r\nFrom: \"\" <sip:0000000000@192.168.1.45>;tag=vQ763mmXmygHQ\r\nTo: <sip:alice@192.168.1.176>\r\nCall-ID: 48f3d911-7340-4888-a33f-0ad10f6d0a37\r\nCSeq: 30461563 INVITE\r\nContact: <sip:mod_sofia@192.168.1.45:5060>\r\nUser-Agent: The 2600hz Project\r\nAllow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, UPDATE, INFO, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE\r\nSupported: precondition, path, replaces\r\nAllow-Events: talk, hold, presence, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer\r\nContent-Type: application/sdp\r\nContent-Disposition: session\r\nContent-Length: 420\r\nX-FS-Support: update_display,send_info\r\nRemote-Party-ID: <sip:0000000000@192.168.1.45>;party=calling;screen=yes;privacy=off\r\n\r\nv=0\r\no=FreeSWITCH 1341566127 1341566128 IN IP4 192.168.1.45\r\ns=FreeSWITCH\r\nc=IN IP4 192.168.1.45\r\nt=0 0\r\nm=audio 29018 RTP/AVP 99 100 9 0 8 3 101 13\r\na=rtpmap:99 G7221/32000\r\na=fmtp:99 bitrate=48000\r\na=rtpmap:100 G7221/16000\r\na=fmtp:100 bitrate=32000\r\na=rtpmap:101 telephone-event/8000\r\na=fmtp:101 0-16\r\na=ptime:20\r\nm=video 26066 RTP/AVP 31 34 98\r\na=rtpmap:31 H261/90000\r\na=rtpmap:34 H263/90000\r\na=rtpmap:98 H264/90000\r\n">>,

    {ok, Req} = parse_sip_packet(SIP),

    ?assertEqual('INVITE', smoke_sip_req:method(Req)),
    ?assertEqual(<<"sip:alice@192.168.1.176">>, smoke_sip_req:pp_uri(smoke_sip_req:request_uri(Req), binary)),
    ?assertEqual(<<"\"\" <sip:0000000000@192.168.1.45>;tag=vq763mmxmyghq">>, smoke_sip_req:pp_uri(smoke_sip_req:header(Req, 'From'), binary)),
    ?assertEqual({<<"application">>, <<"sdp">>, []}, smoke_sip_req:header(Req, 'Content-Type')),
    ?assertEqual({30461563, 'INVITE'}, smoke_sip_req:header(Req, 'CSeq')),

    ContentLength = smoke_sip_req:header(Req, 'Content-Length'),
    ReqBody = smoke_sip_req:request_body(Req),
    ?assertEqual(420, ContentLength),
    ?assertEqual(ContentLength, byte_size(ReqBody)),

    ?assertEqual(<<"v=0\r\no=FreeSWITCH 1341566127 1341566128 IN IP4 192.168.1.45\r\ns=FreeSWITCH\r\nc=IN IP4 192.168.1.45\r\nt=0 0\r\nm=audio 29018 RTP/AVP 99 100 9 0 8 3 101 13\r\na=rtpmap:99 G7221/32000\r\na=fmtp:99 bitrate=48000\r\na=rtpmap:100 G7221/16000\r\na=fmtp:100 bitrate=32000\r\na=rtpmap:101 telephone-event/8000\r\na=fmtp:101 0-16\r\na=ptime:20\r\nm=video 26066 RTP/AVP 31 34 98\r\na=rtpmap:31 H261/90000\r\na=rtpmap:34 H263/90000\r\na=rtpmap:98 H264/90000\r\n">>, ReqBody).

sip_packet_invite_speedtest() ->
        SIP = <<"INVITE sip:alice@192.168.1.176 SIP/2.0\r\nVia: SIP/2.0/UDP 192.168.1.45;rport;branch=z9hG4bK2mBSta3rrB99D\r\nMax-Forwards: 70\r\nFrom: \"\" <sip:0000000000@192.168.1.45>;tag=vQ763mmXmygHQ\r\nTo: <sip:alice@192.168.1.176>\r\nCall-ID: 48f3d911-7340-4888-a33f-0ad10f6d0a37\r\nCSeq: 30461563 INVITE\r\nContact: <sip:mod_sofia@192.168.1.45:5060>\r\nUser-Agent: The 2600hz Project\r\nAllow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, UPDATE, INFO, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE\r\nSupported: precondition, path, replaces\r\nAllow-Events: talk, hold, presence, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer\r\nContent-Type: application/sdp\r\nContent-Disposition: session\r\nContent-Length: 420\r\nX-FS-Support: update_display,send_info\r\nRemote-Party-ID: <sip:0000000000@192.168.1.45>;party=calling;screen=yes;privacy=off\r\n\r\nv=0\r\no=FreeSWITCH 1341566127 1341566128 IN IP4 192.168.1.45\r\ns=FreeSWITCH\r\nc=IN IP4 192.168.1.45\r\nt=0 0\r\nm=audio 29018 RTP/AVP 99 100 9 0 8 3 101 13\r\na=rtpmap:99 G7221/32000\r\na=fmtp:99 bitrate=48000\r\na=rtpmap:100 G7221/16000\r\na=fmtp:100 bitrate=32000\r\na=rtpmap:101 telephone-event/8000\r\na=fmtp:101 0-16\r\na=ptime:20\r\nm=video 26066 RTP/AVP 31 34 98\r\na=rtpmap:31 H261/90000\r\na=rtpmap:34 H263/90000\r\na=rtpmap:98 H264/90000\r\n">>,

    Now = erlang:now(),
    {ok, _} = parse_sip_packet(SIP),
    Elapsed = wh_util:elapsed_us(Now),
    lager:info("parsing took ~p us", [Elapsed]).

-endif.
