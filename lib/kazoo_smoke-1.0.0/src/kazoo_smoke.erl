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

start_link() ->
    start_deps(),
    kazoo_smoke_sup:start_link().

start() ->
    start_deps(),
    application:start(kazoo_smoke).

stop() ->
    application:stop(kazoo_smoke).

start_listener(Ref, _N, smoke_udp, TransOpts, ProtoOpts) ->
    supervisor:start_child(smoke_udp_sup, child_spec(Ref, TransOpts, ProtoOpts));
start_listener(Ref, NumAcceptors, Transport, TransOpts, ProtoOpts)
  when is_integer(NbAcceptors) andalso
       is_atom(Transport) andalso
       is_atom(Protocol) ->
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
