%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% A SIP protocol handler, largely based around the 
%%% cowboy_http_protocol.erl construct
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_sip_protocol).

-behaviour(cowboy_protocol).

-export([start_link/4]). %% API.
-export([init/4, parse_request/1, handler_loop/3]). %% FSM.

-include_lib("whistle/include/sip.hrl").

-record(state, {
          listener :: pid()
         ,socket :: inet:socket()
         ,transport :: module()
         ,handler :: {module(), any()}
         ,onrequest :: fun((#sip_req{}) -> #sip_req{})
         ,onresponse :: fun((wh_sip:status(), wh_sip:headers(), #sip_req{}) -> #sip_req{})
         ,hibernate = false :: boolean()

         ,req_empty_lines = 0 :: integer()
         ,max_empty_lines :: integer()
         ,req_keepalive = 1 :: integer()
         ,max_keepalive :: integer()
         ,max_line_length :: integer()

         ,timeout :: timeout()
         ,dispatch :: cowboy_dispatcher:dispatch_rules()
         ,buffer =  <<>> :: binary()
         ,loop_timeout = infinity :: timeout()
         ,loop_timeout_ref :: undefined | reference()
        }).

%% API

%% @doc Start a SIP protocol process
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
        Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
        {ok, Pid}.

%% FSM
%% @private
-spec init/4 :: (pid(), inet:socket(), module(), any()) -> 'ok'.
init(ListenerPid, Socket, Transport, Opts) ->
    Dispatch = proplists:get_value(dispatch, Opts, []),

    MaxEmptyLines = proplists:get_value(max_empty_lines, Opts, 5),
    MaxKeepalive = proplists:get_value(max_keepalive, Opts, infinity),
    MaxLineLength = proplists:get_value(max_line_length, Opts, 4096),

    OnRequest = props:get_value(onrequest, Opts),
    OnResponse = props:get_value(onresponse, Opts),

    Timeout = props:get_value(timeout, Opts, 5000),

    ok = cowboy:accept_ack(ListenerPid),
    wait_request(#state{listener=ListenerPid
                        ,socket=Socket
                        ,transport=Transport
                        ,dispatch=Dispatch
                        ,max_empty_lines=MaxEmptyLines
                        ,max_keepalive=MaxKeepalive
                        ,max_line_length=MaxLineLength
                        ,timeout=Timeout
                        ,onrequest=OnRequest
                        ,onresponse=OnResponse
                       }).

-spec wait_request/1 :: (#state{}) -> 'ok'.
wait_request(#state{socket=Socket
                    ,transport=Transport
                    ,timeout=T
                    ,buffer=Buffer
                   }=State) ->
    case Transport:recv(Socket, 0, T) of
        {ok, Data} -> parse_request(State#state{buffer = <<Buffer/binary, Data/binary>>});
        {error, _Reason} -> terminate(State)
    end.

%% @private
-spec parse_request/1 :: (#state{}) -> 'ok'.
%% We limit the length of the Request-line to MaxLength to avoid endlessly
%% reading from the socket and eventually crashing.
parse_request(#state{buffer=Buffer, max_line_length=MaxLength}=State) ->
    case erlang:decode_packet(http_bin, Buffer, []) of
        {ok, Request, Rest} -> request(Request, State#state{buffer=Rest});
        {more, _Length} when byte_size(Buffer) > MaxLength ->
            error_terminate(413, State);
        {more, _Length} -> wait_request(State);
        {error, _Reason} -> error_terminate(400, State)
    end.

-spec request/2 :: ({'http_request', wh_sip:method(), cowboy_http:uri(), wh_sip:version()}
                    ,#state{}) -> 'ok'.
request({http_request, _Method, _URI, Version}, State) when Version =/= {2, 0} ->
    error_terminate(505, State);
%% We still receive the original Host header.
request({http_request, Method, '*', Version},
        #state{socket=Socket
               ,transport=Transport
               ,onresponse=OnResponse
              }=State) ->
    lager:debug("recv request ~p: ~p", [Method, Version]),

    parse_header(#sip_req{socket=Socket
                          ,transport=Transport
                          ,connection=close
                          ,pid=self()
                          ,method=Method
                          ,version=Version
                          ,onresponse=OnResponse
                         }, State);
request({http_request, _Method, _URI, _Version}, State) ->
    error_terminate(501, State);
request({http_error, <<"\r\n">>},
        #state{req_empty_lines=N, max_empty_lines=N}=State) ->
    error_terminate(400, State);
request({http_error, <<"\r\n">>}, #state{req_empty_lines=N}=State) ->
    parse_request(State#state{req_empty_lines=N + 1});
request(_Any, State) ->
    lager:debug("no handler for request clause: ~p", [_Any]),
    error_terminate(400, State).

-spec parse_header/2 :: (#sip_req{}, #state{}) -> 'ok'.
parse_header(#sip_req{}=Req, #state{buffer=Buffer
                                    ,max_line_length=MaxLength
                                   }=State) ->
    case erlang:decode_packet(httph_bin, Buffer, []) of
        {ok, Header, Rest} -> header(Header, Req, State#state{buffer=Rest});
        {more, _Length} when byte_size(Buffer) > MaxLength ->
            error_terminate(413, State);
        {more, _Length} -> wait_header(Req, State);
        {error, _Reason} -> error_terminate(400, State)
    end.

-spec wait_header/2 :: (#sip_req{}, #state{}) -> 'ok'.
wait_header(Req, #state{socket=Socket
                        ,transport=Transport
                        ,timeout=T
                        ,buffer=Buffer
                       }=State) ->
    case Transport:recv(Socket, 0, T) of
        {ok, Data} -> parse_header(Req, State#state{
                                          buffer = <<Buffer/binary, Data/binary>>
                                         });
        {error, timeout} -> error_terminate(408, State);
        {error, closed} -> terminate(State)
    end.

-spec header({http_header, integer(), cowboy_http:header(), any(), binary()}
             | http_eoh, #sip_req{}, #state{}) -> ok.
header({http_header, _I, Field, _R, Value}, Req, State) ->
    lager:debug("header ~s: ~s", [Field, Value]),
    Field2 = format_header(Field),
    parse_header(Req#sip_req{headers=[{Field2, Value}|Req#sip_req.headers]},
                 State);
header(http_eoh, Req, #state{buffer=Buffer}=State) ->
    lager:debug("end of headers"),
    onrequest(Req#sip_req{buffer=Buffer}, State#state{buffer= <<>>});
header(_Any, _Req, State) ->
    lager:debug("unknown header: ~p", [_Any]),
    error_terminate(400, State).

%% Call the global onrequest callback. The callback can send a reply,
%% in which case we consider the request handled and move on to the next
%% one. Note that since we haven't dispatched yet, we don't know the
%% handler, host_info, path_info or bindings yet.
-spec onrequest(#sip_req{}, #state{}) -> ok.
onrequest(Req, State=#state{onrequest=undefined}) ->
    dispatch(Req, State);
onrequest(Req, State=#state{onrequest=OnRequest}) ->
    Req2 = OnRequest(Req),
    dispatch(Req2, State).

dispatch(_Req, #state{}=State) ->
    terminate(State).

handler_loop(_,_,_) ->
    ok.

%% Only send an error reply if there is no resp_sent message.
-spec error_terminate(cowboy_http:status(), #state{}) -> ok.
error_terminate(Code, State=#state{socket=Socket, transport=Transport,
                onresponse=OnResponse}) ->
    lager:debug("error termination with code: ~p", [Code]),
    receive
        {cowboy_http_req, resp_sent} -> ok
    after 0 ->
            _ = cowboy_http_req:reply(Code, #sip_req{
                                        socket=Socket
                                        ,transport=Transport
                                        ,onresponse=OnResponse
                                        ,connection=close
                                        ,pid=self()
                                       }),
            ok
    end,
    terminate(State).

-spec terminate(#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
    Transport:close(Socket),
    ok.

%% Internal.

%% @todo While 32 should be enough for everybody, we should probably make
%%       this configurable or something.
-spec format_header(atom()) -> atom(); (binary()) -> binary().
format_header(Field) when is_atom(Field) ->
        Field;
format_header(Field) when byte_size(Field) =< 20; byte_size(Field) > 32 ->
        Field;
format_header(Field) ->
        format_header(Field, true, <<>>).

-spec format_header(binary(), boolean(), binary()) -> binary().
format_header(<<>>, _Any, Acc) ->
        Acc;
%% Replicate a bug in OTP for compatibility reasons when there's a - right
%% after another. Proper use should always be 'true' instead of 'not Bool'.
format_header(<< $-, Rest/bits >>, Bool, Acc) ->
        format_header(Rest, not Bool, << Acc/binary, $- >>);
format_header(<< C, Rest/bits >>, true, Acc) ->
        format_header(Rest, false, << Acc/binary, (cowboy_bstr:char_to_upper(C)) >>);
format_header(<< C, Rest/bits >>, false, Acc) ->
        format_header(Rest, false, << Acc/binary, (cowboy_bstr:char_to_lower(C)) >>).
