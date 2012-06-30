%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_udp).

-export([name/0]).
-export([messages/0]).
-export([connect/3]).
-export([listen/1]).
-export([accept/2]).
-export([recv/3]).
-export([send/4]).
-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([close/1]).
-export([sockname/1]).

%% @doc Name of this transport API, <em>udp</em>.
-spec name() -> udp.
name() -> udp.

%% @doc Atoms used in the process messages sent by this API.
%%
%% They identify incoming data, closed connection and errors when receiving
%% data in active mode.
-spec messages() -> {udp, udp_closed, udp_error}.
messages() -> {udp, udp_closed, udp_error}.

%% @private
-spec connect(string(), inet:port_number(), any())
             -> {ok, inet:socket()} | {error, atom()}.
connect(_Host, Port, Opts) when is_list(_Host), is_integer(Port) ->
    gen_udp:open(Port, Opts ++ [binary, {active, false}, {packet, raw}]).

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
-spec listen([{port, inet:port_number()} | {ip, inet:ip_address()}])
            -> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
    {port, Port} = lists:keyfind(port, 1, Opts),
    Backlog = proplists:get_value(backlog, Opts, 1024),
    ListenOpts0 = [binary, {active, false},
                   {backlog, Backlog}, {packet, raw}, {reuseaddr, true}],
    ListenOpts =
                case lists:keyfind(ip, 1, Opts) of
                    false -> ListenOpts0;
                    Ip -> [Ip|ListenOpts0]
                end,
    gen_udp:open(Port, ListenOpts).

%% @doc Accept an incoming connection on a listen socket.
%% @see gen_udp:accept/2
-spec accept(inet:socket(), timeout())
        -> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, _Timeout) ->
    {ok, LSocket}.

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_udp:recv/3
-spec recv(inet:socket(), non_neg_integer(), timeout())
          -> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
    gen_udp:recv(Socket, Length, Timeout).

%% @doc Send a packet on a socket.
%% @see gen_udp:send/4
-spec send(inet:socket(), any(), any(), iolist()) -> ok | {error, atom()}.
send(Socket, IP, Port, Packet) ->
    gen_udp:send(Socket, IP, Port, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(inet:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_udp:controlling_process/2
-spec controlling_process(inet:socket(), pid())
                         -> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
    gen_udp:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see inet:peername/1
-spec peername(inet:socket())
              -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
    inet:peername(Socket).

%% @doc Close a UDP socket.
%% @see gen_udp:close/1
-spec close(inet:socket()) -> ok.
close(Socket) ->
    gen_udp:close(Socket).

%% @doc Get the local address and port of a socket
%% @see inet:sockname/1
-spec sockname(inet:socket())
              -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
    inet:sockname(Socket).
