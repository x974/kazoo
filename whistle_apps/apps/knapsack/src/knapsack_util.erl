%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knapsack_util).

-export([maybe_save_to_owner/3, maybe_save_to_owner/4
         ,save_to_account/3, save_to_account/4
         ,should_save_type/2
        ]).

%% Is this type of file configured to be saved in this provider?
should_save_type(Settings, Type) ->
    case wh_json:get_value(<<"storage_types">>, Settings) of
        undefined -> true;
        [] -> false;
        L -> lists:member(Type, L)
    end.

%% If we get an owner ID and we successfully open the owner's document, continue
%% Arity 3 will pull the type off the ToSaveId doc; arity 4 sets the type (which
%% may not correspond with doc's pvt_type).
maybe_save_to_owner(_, undefined, _) -> false;
maybe_save_to_owner(AcctDb, OwnerId, ToSaveId) ->
    case couch_mgr:open_doc(AcctDb, OwnerId) of
        {ok, OwnerJObj} -> save_to_owner(AcctDb, OwnerJObj, ToSaveId);
        {error, _} -> false
    end.

maybe_save_to_owner(_, undefined, _, _) -> false;
maybe_save_to_owner(AcctDb, OwnerId, ToSaveId, Type) ->
    case couch_mgr:open_doc(AcctDb, OwnerId) of
        {ok, OwnerJObj} -> save_to_owner(AcctDb, OwnerJObj, ToSaveId, Type);
        {error, _} -> false
    end.

save_to_account(AcctDb, AcctJObj, ToSaveId) ->
    save_to_owner(AcctDb, AcctJObj, ToSaveId).
save_to_account(AcctDb, AcctJObj, ToSaveId, Type) ->
    save_to_owner(AcctDb, AcctJObj, ToSaveId, Type).

%% If the owner has the right pvt key, try to save to each configured provider
%% If any of them are successful, return true; otherwise return false.
save_to_owner(AcctDb, OwnerJObj, ToSaveId) ->
    case wh_json:get_value(<<"pvt_offsite_storage">>, OwnerJObj) of
        undefined -> false;
        OffsiteJObj ->
            lists:any([save(Provider, Settings, AcctDb, ToSaveId)
                       || {Provider, Settings} <- wh_json:to_list(OffsiteJObj)
                      ])
    end.

save_to_owner(AcctDb, OwnerJObj, ToSaveId, Type) ->
    case wh_json:get_value(<<"pvt_offsite_storage">>, OwnerJObj) of
        undefined -> false;
        OffsiteJObj ->
            lists:any([save(Provider, Settings, AcctDb, ToSaveId, Type)
                       || {Provider, Settings} <- wh_json:to_list(OffsiteJObj)
                      ])
    end.

save(Provider, Settings, AcctDb, ToSaveId) ->
    case wh_util:try_load_module(<<"knap_", Provider/binary>>) of
        false -> lager:debug("no such provider: ~s", [Provider]), false;
        Module ->
            {Doc, Type} = load_to_save_doc(AcctDb, ToSaveId),
            try Module:save(Settings, AcctDb, Doc, Type) of
                DidSave -> DidSave
            catch
                _E:_R ->
                    lager:debug("failed to save in ~s: ~s:~p", [Module, _E, _R]),
                    false
            end
    end.
save(Provider, Settings, AcctDb, ToSaveId, Type) ->
    case wh_util:try_load_module(<<"knap_", Provider/binary>>) of
        false -> lager:debug("no such provider: ~s", [Provider]), false;
        Module ->
            {Doc, _} = load_to_save_doc(AcctDb, ToSaveId),
            try Module:save(Settings, AcctDb, Doc, Type) of
                DidSave -> DidSave
            catch
                _E:_R ->
                    lager:debug("failed to save in ~s: ~s:~p", [Module, _E, _R]),
                    false
            end
    end.

load_to_save_doc(AcctDb, Id) ->
    case couch_mgr:open_doc(AcctDb, Id) of
        {ok, Doc} ->
            {Doc, wh_json:get_value(<<"pvt_type">>, Doc)};
        {error, _E} ->
            lager:debug("failed to fetch to_save doc ~s: ~p", [Id, _E]),
            throw(_E)
    end.
