%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(sack_boxdotnet).

-export([save/4]).

-include("knapsack.hrl").

-define(BASE_URL, <<"https://www.box.com/api/2.0">>).

save(Settings, AcctDb, SaveDoc, Type) ->
    Tests = [fun() -> knapsack_util:should_save_type(Settings, Type) end
            ],

    case lists:all(fun(F) -> F() end, Tests) of
        false -> false;
        true -> save_doc(Settings, AcctDb, SaveDoc, Type)
    end.

save_doc(Settings, AcctDb, SaveDoc, Type) ->
    {ok, TokenJObj} = create_token(Settings),
    AuthToken = wh_json:get_value(<<"token">>, TokenJObj),
    Authz = authz_header(ApiKey, AuthToken),

    RootFolderId = wh_json:get_value([<<"item">>, <<"id">>], TokenJObj),

    {ok, DestFolder} = get_destination_folder(wh_json:get_value(<<"api_key">>, Settings)
                                              ,AuthToken
                                              ,RootFolderId
                                              ,Type
                                              ,Authz
                                             ),

    {ok, Binary} = knapsack_util:get_file_to_save(AcctDb, SaveDoc, Type),
    upload_file(DestFolder, Binary, Authz).

upload_file(DestFolder, Binary, Authz) ->
%% https://api.box.com/2.0/files/data \
%% -H "Authorization: BoxAuth api_key=API_KEY&auth_token=AUTH_TOKEN" \
%% -F filename1=@FILE_NAME1 \
%% -F filename2=@FILE_NAME2 \
%% -F folder_id=FOLDER_ID
    Url = filename:join([whapps_config:get(?MODULE, <<"base_url">>, ?BASE_URL)
                         ,<<"files">>
                         ,<<"data">>
                        ]),
    Boundary = wh_util:rand_hex_binary(4),
    {Meta, Binary} = knapsack_util:fetch_file_to_store(AcctDb, SaveDoc, Type),

    ReqBody = [Meta, Boundary

    ReqHeaders = [{<<"Authorization">>, Authz}
                  ,{<<"Content-Type">>, <<"multipart/form-data, boundary=", Boundary/binary>>}
                  ,{<<"Content-Length">>, byte_size(ReqBody)}
                 ]

    case ibrowse:send_req(Url, , 'post', ReqBody) of
        _ -> ok
    end.

-spec get_destination_folder/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                                          {'ok', wh_json:json_object()} |
                                          {'error', integer()}.
get_destination_folder(ApiKey, AuthToken, RootFolderId, Type, Authz) ->
    Url = filename:join([whapps_config:get(?MODULE, <<"base_url">>, ?BASE_URL)
                         ,<<"folders">>
                         ,RootFolderId
                        ]),

    case ibrowse:send_req(Url, [{<<"Authorization">>, Authz}], 'get') of
        {ok, "200", _RespHeaders, RespBody} ->
            lager:debug("root folder properties: ~s", [RespBody]),
            RootFolder = wh_json:decode(RespBody),
            get_destination_folder(RootFolder, Type, Authz);
        {ok, ErrCode, _RespHeaders, _RespBody} ->
            lager:debug("failed to get root folder: ~s: ~s", [ErrCode, _RespBody]),
            {error, wh_util:to_integer(ErrCode)}
    end.

-spec get_destination_folder/3 :: (wh_json:json_object(), ne_binary(), ne_binary()) ->
                                          {'ok', wh_json:json_object()} |
                                          {'error', integer()}.
get_destination_folder(RootFolder, Type, Authz) ->
    Entries = wh_json:get_value([<<"item_collection">>, <<"entries">>], RootFolder, []),
    case [ FolderMeta || FolderMeta <- Entries,
                       wh_json:get_value(<<"type">>, FolderMeta) =:= <<"folder">>,
                       wh_json:get_value(<<"name">>, FolderMeta) =:= Type
         ] of
        [FolderMeta|_] -> {ok, FolderMeta};
        [] -> create_destination_folder(RootFolder, Type, Authz)
    end.

-spec create_destination_folder/3 :: (wh_json:json_object(), ne_binary(), ne_binary()) ->
                                             {'ok', wh_json:json_object()} |
                                             {'error', integer()}.
create_destination_folder(RootFolder, Type, Authz) ->
    RootFolderId = wh_json:get_value(<<"id">>, RootFolder),

    Url = filename:join([whapps_config:get(?MODULE, <<"base_url">>, ?BASE_URL)
                         ,<<"folders">>
                         ,RootFolderId
                        ]),

    ReqBody = wh_json:encode(
                wh_json:from_list([{<<"name">>, Type}])
               ),

    case ibrowse:send_req(Url, [{<<"Authorization">>, Authz}], 'post', ReqBody) of
        {ok, "201", _RespHeaders, RespBody} ->
            lager:debug("created folder ~s: ~s", [Type, RespBody]),
            {ok, wh_json:decode(RespBody)};
        {ok, ErrCode, _RespHeaders, _RespBody} ->
            lager:debug("failed to create folder ~s: ~s:~s", [Type, ErrCode, _RespBody]),
            {error, wh_util:to_integer(ErrCode)}
    end.

create_token(Settings) ->
    ReqBody = wh_json:encode(
                wh_json:from_list([{<<"email">>, wh_json:get_value(<<"email">>, Settings)}])
               ),

    Url = filename:join([whapps_config:get(?MODULE, <<"base_url">>, ?BASE_URL)
                         ,<<"tokens">>
                        ]),

    Authorization = authz_header(wh_json:get_value(<<"api_key">>, Settings)),

    case ibrowse:send_req(Url, [{"Authorization", Authorization}], 'get', ReqBody) of
        {ok, "201", _RespHeaders, RespBody} ->
            lager:debug("created token response: ~s", [RespBody]),
            {ok, wh_json:decode(RespBody)};
        {ok, ErrorCode, _RespHeaders, _RespBody} ->
            lager:debug("failed to create token response: ~s: ~s", [ErrorCode, _RespBody]),
            {error, wh_util:to_integer(ErrorCode)}
    end.

authz_header(ApiKey) ->
    list_to_binary([<<"BoxAuth api_key=">>, ApiKey]).
authz_header(ApiKey, AuthToken) ->
    list_to_binary([<<"BoxAuth api_key=">>, ApiKey, <<"&auth_token=">>, AuthToken]).
