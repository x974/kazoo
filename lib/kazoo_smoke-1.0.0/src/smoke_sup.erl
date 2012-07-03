%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(smoke_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.

-include("smoke.hrl").

%% API.

-spec start_link() -> {'ok', pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

-spec init/1 :: ([]) -> sup_init_ret().
init([]) ->
    Procs = [?CHILD_SUP(smoke_transport_sup)
             ,?CHILD_SUP(smoke_protocol_sup)
            ],
    {ok, {{one_for_one, 10, 10}, Procs}}.
