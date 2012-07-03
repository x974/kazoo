%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(smoke_protocol_sup).

-behaviour(supervisor).

-export([start_link/0
         ,start_request/5
        ]). %% API.
-export([init/1]). %% supervisor.

-include("smoke.hrl").

%% API.

-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_request/5 :: (inet:socket(), module(), module(), wh_proplist(), any()) -> sup_startchild_ret().
start_request(Socket, Transport, Protocol, ProtoOpts, Payload) ->
    supervisor:start_child(?MODULE, {wh_util:rand_hex_binary(10)
                                     ,{Protocol, start_request, [Socket, Transport, ProtoOpts, Payload]}
                                     ,temporary, 5000, worker, [Protocol]
                                    }).

%% supervisor.

-spec init([]) -> {ok, {{one_for_one, 10, 10}, []}}.
init([]) ->
        {ok, {{one_for_one, 10, 10}, []}}.
