%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_resource).

-behaviour(gen_listener).

-export([start_link/1, start_link/2]).
-export([handle_originate_req/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-record(state, {node :: atom()
               ,options :: proplist()
               }).

-define(BINDINGS, [{resource, []}
                   ,{self, []}
                  ]).
-define(RESPONDERS, [{{?MODULE, handle_originate_req}, [{<<"resource">>, <<"originate_req">>}]}]).
-define(QUEUE_NAME, <<"ecallmgr_fs_resource">>).
-define(QUEUE_OPTIONS, [{exclusive, false}]).
-define(CONSUME_OPTIONS, [{exclusive, false}]).
-define(ROUTE_OPTIONS, []).

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
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE, [{bindings, ?BINDINGS}
                                      ,{responders, ?RESPONDERS}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                      ,{route_options, ?ROUTE_OPTIONS}
                                      ,{basic_qos, 1}
                                     ], [Node, Options]).

-spec handle_originate_req/2 :: (wh_json:json_object(), proplist()) -> 'ok' | 'fail'.
handle_originate_req(JObj, Props) ->
    true = wapi_resource:originate_req_v(JObj),
    Node = props:get_value(node, Props),
    lager:debug("received originate request for node ~s", [Node]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
    case wapi_resource:originate_req_v(JObj) of
        false -> 
            lager:debug("originate failed to execute as JObj did not validate", []), 
            E = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, wh_util:to_binary(wh_util:current_tstamp()))}
                 ,{<<"Request">>, JObj}
                 ,{<<"Error-Message">>, <<"originate failed to execute as JObj did not validate">>}
                 | wh_api:default_headers(<<"error">>, <<"originate_resp">>, ?APP_NAME, ?APP_VERSION)
                ],
            wh_api:publish_error(ServerId, E);
        true when Endpoints =:= [] -> 
            lager:debug("originate request had no endpoints", []),
            E = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, wh_util:to_binary(wh_util:current_tstamp()))}
                 ,{<<"Request">>, JObj}
                 ,{<<"Error-Message">>, <<"originate request had no endpoints">>}
                 | wh_api:default_headers(<<"error">>, <<"originate_resp">>, ?APP_NAME, ?APP_VERSION)
                ],
            wh_api:publish_error(ServerId, E);
        true ->
            DialSeparator = case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj, <<"single">>) of
                                <<"simultaneous">> when length(Endpoints) > 1 -> <<",">>;
                                _Else -> <<"|">>
                            end,
            DialStrings = ecallmgr_util:build_bridge_string(Endpoints, DialSeparator),
            Action = get_originate_action(wh_json:get_value(<<"Application-Name">>, JObj), JObj),
            Args = list_to_binary([ecallmgr_fs_xml:get_channel_vars(JObj), DialStrings, " ", Action]),
            _ = ecallmgr_util:fs_log(Node, "whistle originating call: ~s", [Args]),
            case handle_originate_return(Node, freeswitch:api(Node, 'originate', wh_util:to_list(Args))) of
                {ok, Resp} -> 
                    lager:debug("originate completed, sending notice to requestor", []),
                    wapi_resource:publish_originate_resp(ServerId, Resp);
                {error, Error} -> 
                    E = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, wh_util:to_binary(wh_util:current_tstamp()))}
                         ,{<<"Request">>, JObj}
                         | Error
                        ],
                    wh_api:publish_error(ServerId, E)
            end
    end.

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
init([Node, Options]) ->
    put(callid, Node),
    process_flag(trap_exit, true),
    {ok, #state{node=Node, options=Options}}.

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
handle_info({update_options, NewOptions}, State) ->
    {noreply, State#state{options=NewOptions}, hibernate};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {reply, [{node, Node}]}.

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
terminate(_Reason, #state{node=Node}) ->
    lager:debug("fs resource ~s termination: ~p", [Node, _Reason]).

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
-spec get_originate_action/2 :: (ne_binary(), wh_json:json_object()) -> ne_binary().
get_originate_action(<<"fax">>, JObj) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj),
    <<"&txfax(${http_get(", Data/binary, ")})">>;
get_originate_action(<<"transfer">>, JObj) ->
    case wh_json:get_value([<<"Application-Data">>, <<"Route">>], JObj) of
        undefined -> <<"error">>;
        Route ->
            list_to_binary(["'m:^:", get_unset_vars(JObj), "transfer:", wnm_util:to_e164(Route), " XML context_2' inline"])
    end;
get_originate_action(<<"bridge">>, JObj) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj),
    case ecallmgr_fs_xml:build_sip_route(Data, wh_json:get_value(<<"Invite-Format">>, Data)) of
        {error, timeout} -> <<"error">>;
        EndPoint ->
            list_to_binary(["'m:^:", get_unset_vars(JObj), "bridge:", EndPoint, "' inline"])
    end;
get_originate_action(_, _) ->
    <<"&park()">>.

-spec get_unset_vars/1 :: (wh_json:json_object()) -> string().
get_unset_vars(JObj) ->
    %% Refactor (Karl wishes he had unit tests here for you to use)
    ExportProps = [{K, <<>>} || K <- wh_json:get_value(<<"Export-Custom-Channel-Vars">>, JObj, [])],

    Export = [K || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2, [], [{<<"Custom-Channel-Vars">>, wh_json:from_list(ExportProps)}])
                       ,([K, _] = string:tokens(binary_to_list(KV), "=")) =/= undefined],
    case [[$u,$n,$s,$e,$t,$: | K] || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2, [], wh_json:to_proplist(JObj))
                                         ,not lists:member(begin [K, _] = string:tokens(binary_to_list(KV), "="), K end, Export)] of
        [] -> "";
        Unset -> [string:join(Unset, "^"), "^"]
    end.

-spec handle_originate_return/2 :: (atom(), {'ok', ne_binary()} | {'error', ne_binary()} | timeout) -> {'ok', proplist()} |
                                                                                                       {'error', proplist()}.

handle_originate_return(Node, {ok, <<"+OK ", CallId/binary>>}) ->
    start_call_handling(Node, binary:replace(CallId, <<"\n">>, <<>>));
handle_originate_return(Node, {error, <<"-ERR ", Error/binary>>}) ->
    lager:debug("originate on ~s resulted in an error: ~s", [Node, Error]),
    create_error_resp(Error);
handle_originate_return(Node, timeout) ->
    lager:debug("originate timed out on ~s", [Node]),
    create_error_resp(<<"originate command timed out">>);
handle_originate_return(Node, Else) ->
    lager:debug("originate on ~s returned an unexpected result: ~s", [Node, Else]),
    create_error_resp(<<"unexpected originate return value">>).

-spec start_call_handling/2 :: (atom(), ne_binary()) -> {'ok', proplist()} |
                                                        {'error', proplist()}.
start_call_handling(Node, CallId) ->
    erlang:monitor_node(Node, true),
    case freeswitch:handlecall(Node, CallId) of
        ok -> 
            lager:debug("listening to originated call ~s events from ~s", [CallId, Node]),
            wait_for_originate(Node, CallId);
        timeout ->
            lager:debug("timed out trying to listen to originated call events from ~s", [Node]),
            create_error_resp(<<"node timedout">>);
        {'error', badsession} ->
            lager:debug("bad session received when setting up originated call listener from ~s", [Node]),
            create_error_resp(<<"badsession received while registering event listener">>);
        _E ->
            lager:debug("failed to setup listener for originated call events from ~s: ~p", [Node, _E]),
            create_error_resp(<<"unexepected return registering event listener">>)
    end.

-spec wait_for_originate/2 :: (atom(), ne_binary()) -> {'ok', proplist()} |
                                                       {'error', proplist()}.
wait_for_originate(Node, CallId) ->
    receive 
        {_, {event, [CallId | Props]}} ->
            case props:get_value(<<"Event-Name">>, Props) of
                <<"CHANNEL_DESTROY">> -> 
                    lager:debug("received channel destroy event", []),
                    create_success_resp(Props);
                _Else -> 
                    wait_for_originate(Node, CallId)
            end;
        {nodedown, _} ->
            lager:debug("lost connection to node ~s", [Node]),
            erlang:monitor_node(Node, false),
            create_error_resp(<<"lost connection to freeswitch node">>);
        _Else ->
            wait_for_originate(Node, CallId)
    end.

-spec create_error_resp/1 :: (ne_binary()) -> {'error', proplist()}.
create_error_resp(Msg) ->
    {error, [{<<"Error-Message">>, binary:replace(Msg, <<"\n">>, <<>>)}
             | wh_api:default_headers(<<"error">>, <<"originate_resp">>, ?APP_NAME, ?APP_VERSION)
            ]}.

-spec create_success_resp/1 :: (ne_binary()) -> {'ok', proplist()}.
create_success_resp(Props) ->
    EventName = props:get_value(<<"Event-Name">>, Props),
    ApplicationName = props:get_value(<<"Application">>, Props),
    Builders = [fun(P) -> 
                        [{<<"Event-Category">>, <<"resource">>}
                         | props:delete(<<"Event-Category">>, P)
                        ] 
                end
                ,fun(P) -> 
                         [{<<"Event-Name">>, <<"originate_resp">>}
                          | props:delete(<<"Event-Name">>, P)
                         ] 
                 end
                ,fun(_) -> ecallmgr_call_events:create_event(EventName, ApplicationName, Props) end
               ],
    {ok, lists:foldr(fun(F, P) -> F(P) end, [],  Builders)}.