%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% Handle publishing routes, if present
%%%
%%% @end
%%% Created : 27 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wnm_routes).

-export([save/1]).
-export([delete/1]).
-export([get_number_servers/0]).

-include_lib("whistle_number_manager/src/wh_number_manager.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will 
%% produce route updates when appropriate
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (wnm_number()) -> wnm_number().
save(#number{state = <<"reserved">>} = Number) ->
    maybe_publish_routes(Number);
save(#number{state = <<"in_service">>} = Number) ->
    maybe_publish_routes(Number);
save(#number{state = <<"port_in">>} = Number) ->
    maybe_publish_routes(Number);
save(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (wnm_number()) -> wnm_number().
delete(#number{features=Features}=N) -> 
    N#number{features=sets:del_element(<<"routes">>, Features)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_publish_routes/1 :: (wnm_number()) -> wnm_number().
maybe_publish_routes(#number{number=Number, number_doc=JObj, features=Features}=N) ->
    case wh_json:get_ne_value([<<"routes">>, <<"servers">>], JObj) of
        undefined -> N;
        Servers ->
            wh_routes:sync_number(Number, Servers),
            N#number{features=sets:add_element(<<"routes">>, Features)}
    end.

get_number_servers() ->
    lists:foldr(fun(NumberDb, S) ->
                        Db = wh_util:to_binary(http_uri:encode(wh_util:to_list(NumberDb))),
                        get_number_servers(Db, S)
                end, [], wnm_util:get_all_number_dbs()).

get_number_servers(NumberDb, Servers) ->
    ViewOptions = [],
    case couch_mgr:get_results(NumberDb, <<"routes/servers">>, ViewOptions) of
        {error, _R} -> Servers;
        {ok, JObjs} ->
            lists:foldr(fun(JObj, S) ->
                                [{wh_json:get_value(<<"id">>, JObj)
                                  ,wh_json:get_value(<<"value">>, JObj)
                                 } | S
                                ]
                        end, Servers, JObjs)
    end.
