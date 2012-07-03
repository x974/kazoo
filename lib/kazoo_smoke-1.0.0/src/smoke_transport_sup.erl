%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(smoke_transport_sup).

-behaviour(supervisor).

-export([start_link/0
         ,start_transport/5
        ]). %% API.
-export([init/1]). %% supervisor.

-include("smoke.hrl").

%% API.

-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_transport/5 :: (atom(), module(), wh_proplist(), module(), wh_proplist()) -> sup_startchild_ret().
start_transport(Ref, Transport, TransOpts, Protocol, ProtoOpts) ->
    supervisor:start_child(?MODULE, {Ref
                                     ,{Transport, start_link, [TransOpts, Protocol, ProtoOpts]}
                                     ,permanent, 5000, worker, [Transport]
                                    }).

%% supervisor.

-spec init([]) -> {ok, {{one_for_all, 10, 10}, []}}.
init([]) ->
        {ok, {{one_for_all, 10, 10}, []}}.
