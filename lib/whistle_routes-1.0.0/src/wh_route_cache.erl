%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_route_cache).

-behaviour(gen_server).

-export([start_link/2]).
-export([status/1]).
-export([reconnect/1]).
-export([initialize_cache/1]).
-export([sync_itsp_ips/1]).
-export([sync_kazoo_ips/1]).
-export([sync_available_routes/1]).
-export([sync_numbers/1]).
-export([set/3, set/5]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {name
                ,host
                ,port
                ,socket
                ,connected = false
                ,initialized = false
               }).

-include_lib("whistle_routes/src/wh_routes.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/2 :: (text(), text()) -> startlink_ret().
start_link(Host, Port) when not is_list(Host) ->
    start_link(wh_util:to_list(Host), Port);
start_link(Host, Port) when not is_integer(Port) ->
    start_link(Host, wh_util:to_integer(Port));
start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

-spec status/1 :: (pid()) -> proplist() | {'error', _}.
status(Srv) ->
    gen_server:call(Srv, {status}).

-spec reconnect/1 :: (pid()) -> 'ok'.
reconnect(Srv) ->
    gen_server:cast(Srv, {reconnect}).

-spec initialize_cache/1 :: (pid()) -> 'ok'.
initialize_cache(Srv) ->
    ok = sync_itsp_ips(Srv),
    ok = sync_kazoo_ips(Srv),
    ok = sync_available_routes(Srv),
    ok = sync_numbers(Srv).

-spec sync_itsp_ips/1 :: (pid()) -> 'ok'.
sync_itsp_ips(Srv) ->
    Updates = wh_route_util:itsp_ips(),
    gen_server:cast(Srv, {multiset, Updates}).

-spec sync_kazoo_ips/1 :: (pid()) -> 'ok'.
sync_kazoo_ips(Srv) ->
    Updates = wh_route_util:kazoo_ips(),
    gen_server:cast(Srv, {multiset, Updates}).

-spec sync_available_routes/1 :: (pid()) -> 'ok'.
sync_available_routes(Srv) ->
    Updates = wh_route_util:available_kazoo_routes(),
    gen_server:cast(Srv, {multiset, Updates}).

-spec sync_numbers/1 :: (pid()) -> 'ok'.
sync_numbers(Srv) ->
    Updates = wh_route_util:numbers_routes(),
    gen_server:cast(Srv, {multiset, Updates}).

-spec set/3 :: (pid(), text(), binary()) -> 'ok'.
-spec set/5 :: (pid(), text(), integer(), integer(), binary()) -> 'ok'.

set(Srv, Key, Bytes) ->
    set(Srv, Key, 1, 0, Bytes).

set(Srv, Key, Flags, Expire, Bytes) ->
    gen_server:cast(Srv, {set, Key, Flags, Expire, Bytes}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Host, Port]) ->
    self() ! {maintain_socket},
    Name = <<(wh_util:to_binary(Host))/binary, ":", (wh_util:to_binary(Port))/binary>>,
    {ok, #state{name=Name, host=Host, port=Port}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({status}, _From, State) ->
    Resp = [{host, State#state.host}
            ,{port, State#state.host}
            ,{connected, State#state.connected}
            ,{initialized, State#state.initialized}
           ],
    {reply, {ok, Resp}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({reconnect}, #state{socket=Socket}=State) when is_port(Socket) ->
    catch gen_tcp:close(Socket),
    self() ! {maintain_socket},
    {noreply, State#state{connected=false, initialized=false, socket=undefined}};
handle_cast({reconnect}, State) ->
    self() ! {maintain_socket},
    {noreply, State#state{connected=false, initialized=false}};
handle_cast(_, #state{connected=false}=State) ->
    {noreply, State};
handle_cast({multiset, Props}, #state{name=Name, socket=Socket}=State) ->
    case 
        lists:all(fun({Key, Value}) ->
                          lager:debug("setting key '~s' to '~s' in cache ~s", [Key, Value, Name]),
                          try_memcached_fun(set, [{socket, Socket}, Key, 1, 0, Value])
                  end, Props)
    of
        true -> {noreply, State};
        false -> {noreply, State#state{connected=false}}
    end;
handle_cast({set, Key, Flags, Expire, Bytes}, #state{name=Name, socket=Socket}=State) ->
    lager:debug("setting key '~s' to '~s' in cache ~s", [Key, Bytes, Name]),
    case try_memcached_fun(set, [{socket, Socket}, Key, Flags, Expire, Bytes]) of 
        true -> {noreply, State};
        false -> {noreply, State#state{connected=false}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({maintain_socket}, #state{name=Name, socket=Socket, initialized=Init}=State) when is_port(Socket) ->
    try memcached:version({socket, Socket}) of
        [_V] when not Init-> 
            lager:debug("connected to SBC memcache ~s version ~s", [Name, _V]),
            initialize_cache(self()),
            erlang:send_after(5000, self(), {maintain_socket}),
            {noreply, State#state{connected=true, initialized=true}};
        [_V] -> 
            erlang:send_after(5000, self(), {maintain_socket}),
            {noreply, State#state{connected=true}}
    catch
        _:_R ->
            catch gen_tcp:close(Socket),
            lager:debug("failed to get SBC memcache ~s version: ~p", [Name, _R]),
            erlang:send_after(5000, self(), {maintain_socket}),
            {noreply, State#state{connected=false, initialized=false, socket=undefined}}
    end;
handle_info({maintain_socket}, #state{host=Host, port=Port}=State) ->
    case gen_tcp:connect(Host, Port, [binary, {active, false}]) of
        {error, _R} ->
            lager:debug("failed to connect to SBC memcache at ~s port ~p: ~p", [Host, Port, _R]),
            erlang:send_after(5000, self(), {maintain_socket}),
            {noreply, State#state{connected=false, initialized=false}};
        {ok, Socket} ->
            self() ! {maintain_socket},
            {noreply, State#state{socket=Socket, initialized=false}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{name=Name, socket=Socket}) when is_port(Socket) ->
    gen_tcp:close(Socket),
    lager:debug("SBC memcache ~s connection terminating: ~p", [Name, _Reason]);
terminate(_Reason, #state{name=Name}) ->
    lager:debug("SBC memcache ~s connection terminating: ~p", [Name, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec try_memcached_fun/2 :: (atom(), list()) -> boolean().
try_memcached_fun(Function, Args) ->
    try apply(memcached, Function, Args) of
        __ -> true
    catch
        _:_ -> false
    end.
