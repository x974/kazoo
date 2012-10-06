%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knapsack_handlers).

-export([handle_vm/2
         ,handle_fax/2
        ]).

-include("knapsack.hrl").

handle_vm(JObj, _Props) ->
    true = wapi_notifications:voicemail_v(JObj),
    whapps_util:put_callid(JObj),

    AcctDb = wh_json:get_value(<<"Account-DB">>, JObj),
    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),

    MediaId = wh_json:get_value(<<"Voicemail-Name">>, JObj),

    {ok, VMBox} = couch_mgr:open_doc(AcctDb, wh_json:get_value(<<"Voicemail-Box">>, JObj)),
    VMOwnerId = wh_json:get_value(<<"owner_id">>, VMBox),

    {ok, AcctJObj} = couch_mgr:open_cache_doc(AcctDb, AcctId),

    UnownedOnly = wh_json:is_true(<<"pvt_offsite_storage_of_unowned_only">>, AcctJObj, true),
    SavedToOwner = knapsack_util:maybe_save_to_owner(AcctDb, MediaId, VMOwnerId, <<"voicemail">>),

    case not (SavedToOwner and UnownedOnly) of
        true -> knapsack_util:save_to_account(AcctDb, AcctJObj, MediaId, <<"voicemail">>);
        false -> lager:debug("saved to owner, not saving to account")
    end.

handle_fax(JObj, _Props) ->
    true = wapi_notifications:fax_v(JObj),
    whapps_util:put_callid(JObj),

    ok.
