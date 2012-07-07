%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% A SIP protocol handler, largely based around the 
%%% cowboy_http_protocol.erl construct
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(smoke_sip_protocol).

-export([start_request/4]). %% API.

-export([init/4
         ,parse_request/1
         ,handler_loop/3
        ]).

-include("smoke.hrl").

-record(state, {
          socket :: inet:socket()
         ,transport :: module()
         ,handler :: {module(), any()}
         ,onrequest :: fun((smoke_sip:sip_req()) -> smoke_sip:sip_req())
         ,onresponse :: fun((wh_sip:status(), wh_sip:headers(), smoke_sip:sip_req()) -> smoke_sip:sip_req())
         ,hibernate = false :: boolean()

         ,sender_ip :: inet:ip_address()
         ,sender_port :: inet:port_number()

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
-spec start_request/4 :: (inet:socket(), module(), wh_proplist(), any()) -> {'ok', pid()}.
start_request(Socket, Transport, ProtoOpts, Payload) ->
    Pid = spawn_link(?MODULE, init, [Socket, Transport, ProtoOpts, Payload]),
    {ok, Pid}.

%% FSM
%% @private
-spec init/4 :: (inet:socket(), module(), wh_proplist(), any()) -> 'ok'.
init(Socket, Transport, Opts, {SenderIP, SenderPort, Buffer}) ->
    Dispatch = proplists:get_value(dispatch, Opts, []),

    MaxEmptyLines = proplists:get_value(max_empty_lines, Opts, 5),
    MaxKeepalive = proplists:get_value(max_keepalive, Opts, infinity),
    MaxLineLength = proplists:get_value(max_line_length, Opts, 4096),

    OnRequest = props:get_value(onrequest, Opts),
    OnResponse = props:get_value(onresponse, Opts),

    Timeout = props:get_value(timeout, Opts, 5000),

    parse_request(#state{socket=Socket
                         ,transport=Transport
                         ,dispatch=Dispatch
                         ,max_empty_lines=MaxEmptyLines
                         ,max_keepalive=MaxKeepalive
                         ,max_line_length=MaxLineLength
                         ,timeout=Timeout
                         ,onrequest=OnRequest
                         ,onresponse=OnResponse
                         ,sender_ip=SenderIP
                         ,sender_port=SenderPort
                         ,buffer=Buffer
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
    lager:debug("buffer s: ~b(~b)", [byte_size(Buffer), MaxLength]),
    case smoke_sip:parse_sip_packet(Buffer) of
        {error, Code} -> error_terminate(Code, State);
        {ok, Req} -> dispatch(Req, State)
    end.

%% -spec wait_header/2 :: (smoke_sip_req:sip_req(), #state{}) -> 'ok'.
%% wait_header(Req, #state{socket=Socket
%%                         ,transport=Transport
%%                         ,timeout=T
%%                         ,buffer=Buffer
%%                        }=State) ->
%%     case Transport:recv(Socket, 0, T) of
%%         {ok, Data} -> parse_request(Req, State#state{
%%                                            buffer = <<Buffer/binary, Data/binary>>
%%                                           });
%%         {error, timeout} -> error_terminate(408, State);
%%         {error, closed} -> terminate(State)
%%     end.

%% Call the global onrequest callback. The callback can send a reply,
%% in which case we consider the request handled and move on to the next
%% one. Note that since we haven't dispatched yet, we don't know the
%% handler, host_info, path_info or bindings yet.
-spec onrequest(smoke_sip_req:sip_req(), #state{}) -> ok.
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
-spec error_terminate(smoke_sip:sip_status(), #state{}) -> ok.
error_terminate(Code, #state{socket=Socket, transport=Transport
                             ,onresponse=OnResponse
                            }=State) ->
    lager:debug("error termination with code: ~p", [Code]),
    receive
        {cowboy_http_req, resp_sent} -> ok
    after 0 ->
            Req = lists:foldl(fun({M, D}, R) -> smoke_sip_req:M(R, D) end
                              ,smoke_sip_req:new()
                              ,[{set_socket, Socket}
                                ,{set_transport, Transport}
                                ,{set_onresponse, OnResponse}
                               ]),
            _ = smoke_sip_req:reply(Req, Code),
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
