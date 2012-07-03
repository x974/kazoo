%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_smoke).

-export([start_link/0
         ,start/0
         ,stop/0
         ,start_listener/5
        ]).

-include("smoke.hrl").

start_link() ->
    start_deps(),
    lager:debug("started deps"),
    smoke_sup:start_link().

start() ->
    start_deps(),
    lager:debug("started deps"),
    application:start(kazoo_smoke).

stop() ->
    lager:debug("stopped application"),
    application:stop(kazoo_smoke).

start_listener(Ref, _N, smoke_udp_transport, TransOpts, ProtoOpts) ->
    smoke_transport_sup:start_transport(Ref, smoke_udp_transport, TransOpts, smoke_sip_protocol, ProtoOpts);
start_listener(Ref, NumAcceptors, Transport, TransOpts, ProtoOpts)
  when is_integer(NumAcceptors) andalso
       is_atom(Transport) ->
    ranch:start_listener(Ref, NumAcceptors
                         ,Transport, TransOpts
                         ,smoke_sip_protocol, ProtoOpts
                        ).

%% @private
-spec start_deps/0 :: () -> 'ok'.
start_deps() ->
    lager:start(),
    _ = [wh_util:ensure_started(App)
         || App <- [sasl, crypto, gproc, ranch]
        ],
    ok.
