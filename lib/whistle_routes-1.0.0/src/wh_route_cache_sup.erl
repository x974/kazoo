%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_route_cache_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/0]).
-export([caches/0]).
-export([add/2]).
-export([remove/1, remove/2]).
-export([get_name/2]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Name, Type, Args), fun(N, T, A) -> {N, {wh_route_cache, start_link, A}, permanent, 5000, T, []} end(Name, Type, Args)).

-type running_caches() :: [{list(), pid()},...] | [].
-export_type([running_caches/0]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec caches/0 :: () -> running_caches().
caches() ->
    [{Name, PID} 
     || {Name, PID, Type, _} <- supervisor:which_children(?SERVER)
            ,Type =:= worker
    ].

-spec add/2 :: (text(), text()) -> any().
add(Host, Port) ->
    supervisor:start_child(?SERVER, ?CHILD(get_name(Host, Port), worker, [Host, Port])).

-spec remove/1 :: (list()) -> any().
remove(Name) ->
    _ = supervisor:terminate_child(?SERVER, Name),
    supervisor:delete_child(?SERVER, Name).

-spec remove/2 :: (text(), text()) -> any().
remove(Host, Port) ->
    remove(get_name(Host, Port)).

-spec get_name/2 :: (text(), text()) -> list().
get_name(Host, Port) ->
    Name = <<(wh_util:to_binary(Host))/binary, ":", (wh_util:to_binary(Port))/binary>>,
    wh_util:to_list(Name).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [],

    {ok, {SupFlags, Children}}.
