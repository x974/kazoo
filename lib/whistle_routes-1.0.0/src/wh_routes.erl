%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_routes).

-behaviour(gen_server).

-export([start_link/0]).
-export([sync_number/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {}).

-include_lib("whistle_routes/src/wh_routes.hrl").

-type configured_caches() :: [{list(), ne_binary(), integer()},...] | [].

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
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec sync_number/2 :: (ne_binary(), [ne_binary(),...] | []) -> 'ok'.
sync_number(Number, Servers) ->
    Routes = wh_util:join_binary([<<"sip:", Server/binary>> 
                                      || Server <- Servers
                                 ], <<";">>),
    Caches = [PID || {_, PID} <- wh_route_cache_sup:caches()],
    _ = [wh_route_cache:set(Cache, Number, Routes) || Cache <- Caches],
    ok.

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
init([]) ->
    self() ! {maintain_route_caches},
    {ok, #state{}}.

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
handle_info({maintain_route_caches}, State) ->
    _ = sync_configuration(),
    %% TODO: get VCC IPs?
    _ = set_maintenance_timer(),
    {noreply, State};
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
terminate(_Reason, _State) ->
    lager:debug("routes terminating: ~p", [_Reason]).

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
-spec sync_configuration/0 :: () -> 'ok'.
sync_configuration() ->
    Running = wh_route_cache_sup:caches(),
    Configured = get_configured_caches(),
    Removed = start_added_caches(Configured, Running),
    _ = stop_removed_caches(Removed),
    ok.

-spec set_maintenance_timer/0 :: () -> reference().
set_maintenance_timer() ->
    CacheConfigPoll = whapps_config:get_integer(?CONFIG_CAT, <<"cache_config_poll">>, 1800000),
    erlang:send_after(CacheConfigPoll, self(), {maintain_route_caches}).    

-spec get_configured_caches/0 :: () -> configured_caches().
get_configured_caches() ->
    whapps_config:flush(?CONFIG_CAT),
    Caches = whapps_config:get(?CONFIG_CAT, <<"caches">>),
    lists:foldr(fun(K, C) ->
                        case wh_json:get_value([K, <<"host">>], Caches) of    
                            undefined -> C;
                            Host ->
                                Port = wh_json:get_integer_value([K, <<"port">>], Caches, 11211),
                                [{wh_route_cache_sup:get_name(Host, Port), Host, Port}|C]
                        end
                end, [], wh_json:get_keys(Caches)).

-spec stop_removed_caches/1 :: (proplist()) -> 'ok'.
stop_removed_caches([]) -> ok;
stop_removed_caches([{Name, _} | Remove]) ->
    lager:debug("stopping removed cache ~s", [Name]),
    wh_route_cache_sup:remove(Name),
    stop_removed_caches(Remove).

-spec start_added_caches/2 :: (configured_caches(), proplist()) -> proplist().
start_added_caches(Configured, Running) ->
    lists:foldr(fun({Name, Host, Port}, R) ->
                        lager:debug("ensuring cache ~s is maintained", [Name]),
                        _ = wh_route_cache_sup:add(Host, Port),
                        props:delete(Name, R)
                end, Running, Configured).
