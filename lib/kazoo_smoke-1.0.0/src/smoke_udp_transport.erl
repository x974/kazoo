%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Opens and accepts data from the socket, handing the data off to the
%%% protocol handler
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(smoke_udp_transport).

%% API
-export([start_link/3
         ,receiver/3
        ]).

%% Helpers
-export([name/0
         ,messages/0
         ,listen/1
         ,recv/3
         ,send/4
         ,setopts/2
         ,controlling_process/2
         ,peername/1
         ,close/1
         ,sockname/1
         ,posix_to_friendly/1
        ]).

-export_type([payload/0]).

-include("smoke.hrl").

-spec start_link/3 :: (wh_proplist(), module(), wh_proplist()) -> {'ok', pid()}.
start_link(TransOpts, Protocol, ProtoOpts) ->
    lager:debug("starting UDP transport"),
    case catch ?MODULE:listen(TransOpts) of
        {ok, Socket} ->
            lager:debug("listening to socket: ~p", [Socket]),
            Pid = spawn_link(?MODULE, receiver, [Socket, Protocol, ProtoOpts]),
            lager:debug("receiver at ~p", [Pid]),
            {ok, Pid};
        {error, _E}=E ->
            lager:debug("failed to open socket: ~p", [_E]),
            E;
        {'EXIT', R} ->
            ST = erlang:get_stacktrace(),
            lager:debug("crashed while opening socket: ~p", [R]),
            [lager:debug("s: ~p", [S]) || S <- ST],
            {error, R}
    end.

-spec receiver/3 :: (inet:socket(), module(), wh_proplist()) -> 'ok'.
receiver(Socket, Protocol, ProtoOpts) ->
    case ?MODULE:recv(Socket, 0, 5000) of
        {ok, Socket, Payload} ->
            lager:debug("payload recv: ~p", [Payload]),
            smoke_protocol_sup:start_request(Socket, ?MODULE, Protocol, ProtoOpts, Payload),
            receiver(Socket, Protocol, ProtoOpts);
        {error, timeout} -> 
            lager:debug("timed out waiting for a packet, trying again"),
            receiver(Socket, Protocol, ProtoOpts);
        {error, _E} ->
            lager:debug("error waiting for a packet: ~p", [_E]),
            ok
    end.

%% @doc Name of this transport API, <em>udp</em>.
-spec name() -> 'udp'.
name() -> 'udp'.

%% @doc Atoms used in the process messages sent by this API.
%%
%% They identify incoming data, closed connection and errors when receiving
%% data in active mode.
-spec messages() -> {'udp', 'udp_closed', 'udp_error'}.
messages() -> {'udp', 'udp_closed', 'udp_error'}.

%% @doc Setup a socket to listen on the given port on the local host.
%%
%% The available options are:
%% <dl>
%%  <dt>port</dt><dd>Mandatory. UDP port number to open.</dd>
%%  <dt>backlog</dt><dd>Maximum length of the pending connections queue.
%%   Defaults to 1024.</dd>
%%  <dt>ip</dt><dd>Interface to listen on. Listen on all interfaces
%%   by default.</dd>
%% </dl>
%%
%% @see gen_udp:listen/2
-spec listen([{'port', inet:port_number()} |
              {'ip', inet:ip_address()}
             ]) -> {'ok', inet:socket()} |
                   {'error', inet:posix()}.
listen(Opts) ->
    {port, Port} = lists:keyfind(port, 1, Opts),
    lager:debug("starting UDP acceptor on port ~b", [Port]),

    ListenOpts0 = [binary
                   ,{active, false}
                   ,{reuseaddr, true}
                  ],
    ListenOpts =
        case lists:keyfind(ip, 1, Opts) of
            false -> ListenOpts0;
            Ip -> [Ip|ListenOpts0]
        end,

    lager:debug("open UDP port ~b with opts: ~p", [Port, ListenOpts]),

    gen_udp:open(Port, ListenOpts).

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_udp:recv/3
-type payload() :: {inet:ip_address(), inet:port_number(), ne_binary()}.

-spec recv(inet:socket(), non_neg_integer(), timeout()) ->
                  {'ok', inet:socket(), payload()} |
                  {'error', 'closed' | atom()}.
recv(Socket, Length, Timeout) ->
    case gen_udp:recv(Socket, Length, Timeout) of
        {ok, {_SenderIP, _SenderPort, _Packet}=Payload} -> {ok, Socket, Payload};
        {error, _}=E -> E
    end.

%% @doc Send a packet on a socket.
%% @see gen_udp:send/4
-spec send(inet:socket(), any(), any(), iolist()) -> 'ok' | {'error', atom()}.
send(Socket, IP, Port, Packet) ->
    gen_udp:send(Socket, IP, Port, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(inet:socket(), list()) -> 'ok' | {'error', atom()}.
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_udp:controlling_process/2
-spec controlling_process(inet:socket(), pid()) -> 'ok' |
                                                   {'error', 'closed' | 'not_owner' | atom()}.
controlling_process(Socket, Pid) ->
    gen_udp:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see inet:peername/1
-spec peername(inet:socket()) -> {'ok', {inet:ip_address(), inet:port_number()}} |
                                 {'error', atom()}.
peername(Socket) ->
    inet:peername(Socket).

%% @doc Close a UDP socket.
%% @see gen_udp:close/1
-spec close(inet:socket()) -> 'ok'.
close(Socket) ->
    gen_udp:close(Socket).

%% @doc Get the local address and port of a socket
%% @see inet:sockname/1
-spec sockname(inet:socket()) -> {'ok', {inet:ip_address(), inet:port_number()}} |
                                 {'error', atom()}.
sockname(Socket) ->
    inet:sockname(Socket).

-spec posix_to_friendly/1 :: (inet:posix()) -> ne_binary().
posix_to_friendly(Posix) when is_atom(Posix) ->
    wh_util:to_binary(erl_posix_msg:message(Posix)).
