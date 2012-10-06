%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knap_boxdotnet).

-define(BASE_URL, <<"https://api.box.com/2.0">>).

save(Settings, AcctDb, SaveDoc, Type) ->
    Tests = [fun() -> should_save_type(Settings, Type) end
             ,fun() -> test_auth_token(Settings) end
            ],

    case lists:all(fun(F) -> F() end, Tests) of
        false -> false;
        true -> save_doc(Settings, AcctDb, SaveDoc, Type)
    end.

save_doc(Settings, AcctDb, SaveDoc, Type) ->
    ok.

should_save_type(Settings, Type) ->
    case wh_json:get_value(<<"storage_types">>, Settings) of
        undefined -> true;
        [] -> false;
        L -> lists:member(Type, L)
    end.

test_auth_token(Settings) ->
    BaseUrl = whapps_config:get(?MODULE, <<"base_url">>, ?BASE_URL),

    ApiKey = wh_json:get_value(<<"api_key">>, Settings),
    AuthToken = wh_json:get_value(<<"auth_token">>, Settings),

    test_auth_token(BaseUrl, ApiKey, AuthToken).

test_auth_token(Url, ApiKey, AuthToken) ->
    Authorization = list_to_binary([<<"BoxAuth api_key=">>, ApiKey, <<"&auth_token=">>, AuthToken]),
    case ibrowse:send_req(Url, [{"Authorization", Authorization}], 'get') of
        {ok, "200", _RespHeaders, RespBody} ->
            true;
        {ok, _Status, _RespHeaders, RespBody} ->
            false;
        {error, Reason} ->
            false
    end.

open_folder(Settings) ->
    BaseUrl = whapps_config:get(?MODULE, <<"base_url">>, ?BASE_URL),

    ApiKey = wh_json:get_value(<<"api_key">>, Settings),
    AuthToken = wh_json:get_value(<<"auth_token">>, Settings),

    KazooFolder = whapps_config:get(?MODULE, <<"folder_name">>, <<"saved_kazoos">>),
    Url = filename:join([BaseUrl, <<"folders">>, KazooFolder]),

    open_folder(Url, ApiKey, AuthToken).
open_folder(Url, ApiKey, AuthToken) ->
    Authorization = list_to_binary([<<"BoxAuth api_key=">>, ApiKey, <<"&auth_token=">>, AuthToken]),
    case ibrowse:send_req(Url, [{"Authorization", Authorization}], 'get') of
        {error, _E}=E -> E;
        {ok, "200", _RespHeaders, RespBody} ->
            {ok, wh_json:decode(RespBody)};
        {ok, _Status, _RespHeaders, RespBody} ->
            lager:debug("non successful fetch of folder: ~s: ~s", [_Status, RespBody]),
            {error, wh_json:decode(RespBody)}
    end.
