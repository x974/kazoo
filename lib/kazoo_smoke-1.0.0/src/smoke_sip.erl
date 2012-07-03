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
         ,extract_method/2
         ,extract_request_uri/2
         ,headers_supported/0
         ,methods_supported/0
         ,is_known_method/1
        ]).

-include("smoke.hrl").
-include("smoke_sip.hrl").

-export_type([method/0
              ,version/0
              ,header/0
              ,headers/0
              ,status/0
              ,sip_transport/0
              ,sip_req()
              ,sip_uri()
              ,sip_uri_params()
             ]).

-spec version/0 :: () -> 'SIP/2.0'.
version() ->
    'SIP/2.0'.

-spec headers_supported/0 :: () -> headers().
headers_supported() ->
    ['Via', 'To', 'From', 'CSeq', 'Call-ID', % these go in all responses
     'Max-Forwards', 'Contact', 'Content-Type',
     'Content-Length',
     %% Optional Headers
     'Accept', 'Accept-Encoding', 'Accept-Language',
     'Alert-Info', 'Allow', 'Authentication-Info',
     'Authorization', 'Call-Info', 'Content-Disposition',
     'Content-Encoding', 'Content-Language', 'Date',
     'Error-Info', 'Expires', 'In-Reply-To', 'Min-Expires',
     'MIME-Version', 'Organization', 'Priority',
     'Proxy-Authenticate', 'Proxy-Authorization', 'Proxy-Require',
     'Record-Route', 'Reply-To', 'Require', 'Retry-After',
     'Route', 'Server', 'Subject', 'Supported', 'Timestamp',
     'Unsupported', 'User-Agent', 'Warning', 'WWW-Authenticate'
    ].

-spec is_known_method/1 :: (atom() | string() | binary()) -> 'false' | {'true', methods()}.
is_known_method(M) ->
    case catch wh_util:to_atom(M) of
        {'EXIT', _} -> false;
        Matom when is_atom(Matom) ->
            case lists:member(Matom, ?METHODS_SUPPORTED) of
                true -> {true, Matom};
                false -> false
            end
    end.

-spec extract_method/2 :: (sip_req(), ne_binary()) -> {sip_req(), ne_binary()} |
                                           {'error', 501}.
extract_method(SipReq, Buffer0) ->
    {M0, Buffer1} = extract_until(Buffer0, $ ),
    case is_known_method(M) of
        {true, M1} -> {SipReq#sip_req{method=M1}, Buffer1};
        false -> {error, 501}
    end.

extract_request_uri(SipReq, Buffer) ->
    case extract_sip_uri(Buffer) of
        {error, _}=E -> E;
        {SipUri, Buffer1} -> {SipReq#sip_req{request_uri=SipUri}, Buffer1}
    end.
             

-spec extract_sip_uri/1 :: (ne_binary()) -> {sip_uri(), ne_binary()} |
                                            {'error', 400}.
extract_sip_uri(<<"sip:", Buffer/binary>>) ->
    extract_sip_uri(Buffer, <<"sip:">>);
extract_sip_uri(<<"sips:", Buffer/binary>>) ->
    extract_sip_uri(Buffer, <<"sips:">>);
extract_sip_uri(_B) ->
    {error, 400}.

-spec extract_sip_uri/2 :: (ne_binary(), ne_binary()) -> {sip_uri(), ne_binary()} |
                                                         {'error', integer()}.
extract_sip_uri(Buffer, Scheme) ->
    {U, P, Buffer0} = extract_sip_user_pass(Buffer),
    {H, Port, Buffer1} = extract_sip_host_port(Buffer0),
    {Params, Hdrs, Buffer2} = extract_sip_params_headers(Buffer1),
    {#sip_uri{scheme=Scheme
              ,user=U
              ,password=P
              ,host=H
              ,port=Port
              ,params=Params
              ,headers=Hdrs
             }
     ,Buffer2}.

-spec extract_sip_user_pass/1 :: (ne_binary()) -> {ne_binary(), binary(), ne_binary()}.
extract_sip_user_pass(Buffer) ->
    extract_sip_user(Buffer, []).

-spec extract_sip_user/2 :: (ne_binary(), list()) -> {ne_binary(), binary(), ne_binary()}.
extract_sip_user(<<":", Buffer/binary>>, Acc) ->
    %% have a password to extract
    User = lists:reverse(Acc),
    {Pass, Buffer1} = extract_sip_password(Buffer),
    {User, Pass, Buffer1};
extract_sip_user(<<"@", Buffer/binary>>, Acc) ->
    %% no password, at host boundry
    {lists:reverse(Acc), <<>>, Buffer};
extract_sip_user(<<U:1/binary, Buffer>>, Acc) ->
    extract_sip_user(Buffer, [U | Acc]).

-spec extract_sip_pass/1 :: (ne_binary()) -> {binary(), ne_binary()}.
-spec extract_sip_pass/2 :: (ne_binary(), list()) -> {binary(), ne_binary()}.
extract_sip_password(Buffer) ->
    extract_sip_password(Buffer, []).
extract_sip_password(<<"@", Buffer/binary>>, Acc) ->
    {lists:reverse(Acc), Buffer};
extract_sip_password(<<P:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_password(Buffer, [P | Acc]).

-spec extract_sip_host_port/1 :: (ne_binary()) -> {ne_binary(), binary(), ne_binary()}.
extract_sip_host_port(Buffer) ->
    extract_sip_host(Buffer, []).

extract_sip_host(<<":", Buffer/binary>>, Acc) ->
    % have port to extract
    Host = lists:reverse(Acc),
    {Port, Buffer1} = extract_sip_port(Buffer),
    {Host, Port, Buffer1};
extract_sip_host(<<";", Buffer/binary>>, Acc) ->
    % no port, at host params boundry
    {lists:reverse(Acc), <<>>, Buffer};
extract_sip_host(<<" ", Buffer/binary>>, Acc) ->
    % no port, at host params boundry
    {lists:reverse(Acc), <<>>, Buffer};
extract_sip_host(<<"\r\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {lists:reverse(Acc), <<>>, Buffer};
extract_sip_host(<<"\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {lists:reverse(Acc), <<>>, Buffer};
extract_sip_host(<<H:1/binary, Buffer>>, Acc) ->
    extract_sip_host(Buffer, [H | Acc]).

extract_sip_port(Buffer) ->
    extract_sip_port(Buffer, []).
extract_sip_port(<<";", Buffer/binary>>, Acc) ->
    % at params boundry
    {lists:reverse(Acc), Buffer};
extract_sip_port(<<" ", Buffer/binary>>, Acc) ->
    % at end of URI
    {lists:reverse(Acc), Buffer};
extract_sip_port(<<"\r\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {lists:reverse(Acc), Buffer};
extract_sip_port(<<"\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {lists:reverse(Acc), Buffer};
extract_sip_port(<<P:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_port(Buffer, [P | Acc]).

-spec extract_sip_params_headers/1 :: (ne_binary()) -> {ne_binary(), binary(), ne_binary()}.
extract_sip_params_headers(Buffer) ->
    extract_sip_params(Buffer, #sip_uri_params{}).

extract_sip_params(<<"?", Buffer/binary>>, Params) ->
    % at headers boundry
    {Hdrs, Buffer1} = extract_sip_headers(Buffer),
    {Params, Hdrs, Buffer};
extract_sip_params(<<" ", Buffer/binary>>, Params) ->
    % no headers, at line ending
    {Params, #sip_uri_headers{}, Buffer};
extract_sip_params(<<"\r\n", Buffer/binary>>, Params) ->
    % no headers, at line ending
    {Params, #sip_uri_headers{}, Buffer};
extract_sip_params(<<"\n", Buffer/binary>>, Params) ->
    % no headers, at line ending
    {Params, #sip_uri_headers{}, Buffer};
extract_sip_params(<<"transport=", Buffer>>, #sip_uri_params{transport=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_params(Buffer1, Params#sip_uri_params{transport=V});
extract_sip_params(<<"maddr=", Buffer>>, #sip_uri_params{maddr=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_params(Buffer1, Params#sip_uri_params{maddr=V});
extract_sip_params(<<"ttl=", Buffer>>, #sip_uri_params{ttl=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_params(Buffer1, Params#sip_uri_params{ttl=wh_util:to_integer(V)});
extract_sip_params(<<"user=", Buffer>>, #sip_uri_params{user=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_params(Buffer1, Params#sip_uri_params{user=V});
extract_sip_params(<<"lr=", Buffer>>, #sip_uri_params{lr=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_params(Buffer1, Params#sip_uri_params{lr=V});
extract_sip_params(<<"method=", Buffer>>, #sip_uri_params{method=undefined}=Params) ->
    {V, Buffer1} = extract_sip_param_value(Buffer),
    case is_known_method(list_to_binary(V)) of
        {true, M} -> extract_sip_params(Buffer1, Params#sip_uri_params{method=M});
        false -> {error, 400}
    end.

extract_sip_param_value(Buffer) ->
    extract_sip_param_value(Buffer, []).
extract_sip_param_value(<<";", Buffer/binary>>, Acc) ->
    % k/v delimiter
    {lists:reverse(Acc), Buffer};
extract_sip_param_value(<<"?", _/binary>> = Buffer, Acc) ->
    % end of params, start of headers
    {lists:reverse(Acc), Buffer};
extract_sip_param_value(<<" ", _/binary>> = Buffer, Acc) ->
    % end of URI
    {lists:reverse(Acc), Buffer};
extract_sip_param_value(<<"\r\n", _/binary>> = Buffer, Acc) ->
    % end of line
    {lists:reverse(Acc), Buffer};
extract_sip_param_value(<<"\n", _/binary>> = Buffer), Acc ->
    % end of line
    {lists:reverse(Acc), Buffer};
extract_sip_param_value(<<V:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_param_value(Buffer, [V, Acc]).

extract_sip_param_value(Buffer) ->
    extract_sip_headers(Buffer, []).
extract_sip_param_value(<<" ", Buffer/binary>>, Acc) ->
    % at end of URI
    {lists:reverse(Acc), Buffer};
extract_sip_param_value(<<"\r\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {lists:reverse(Acc), Buffer};
extract_sip_param_value(<<"\n", Buffer/binary>>, Acc) ->
    % no port, at line ending
    {lists:reverse(Acc), Buffer};
extract_sip_param_value(<<P:1/binary, Buffer/binary>>, Acc) ->
    extract_sip_param_value(Buffer, [P | Acc]).

extract_sip_headers(Buffer) ->
    extract_sip_headers(Buffer, [], []).

extract_sip_headers(<<" ", _/binary>> = Buffer, Hdrs, _KeyAcc) ->
    {Hdrs, Buffer};
extract_sip_headers(<<"\r\n", _/binary>> = Buffer, Hdrs, _KeyAcc) ->
    {Hdrs, Buffer};
extract_sip_headers(<<"\n", _/binary>> = Buffer, Hdrs, _KeyAcc) ->
    {Hdrs, Buffer};
extract_sip_headers(<<"=", Buffer/binary>>, Hdrs, KeyAcc) ->
    %% Key ended, get value
    {Value, Buffer1} = extract_sip_param_value(Buffer),
    extract_sip_headers(Buffer1, [{lists:reverse(KeyAcc), Value}|Hdrs], []).


extract_until(Buffer, Terminator) ->
    extract_until(Buffer, Terminator, []).
extract_until(<<Terminator, Buffer/binary>>, Terminator, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Buffer};
extract_until(<<T:1/binary, Buffer/binary>>, Terminator, Acc) ->
    extract_until(Buffer, Terminator, [T | Acc]).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

sip_uri_test() ->
    <<"sip:alice@atlanta.com;maddr=239.255.255.1;ttl=15">>
