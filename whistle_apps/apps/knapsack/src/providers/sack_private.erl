%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(sack_private).

-export([save/4]).

-include("knapsack.hrl").

save(Settings, AcctDb, SaveDoc, Type) ->
    Tests = [fun() -> knapsack_util:should_save_type(Settings, Type) end
            ],

    case lists:all(fun(F) -> F() end, Tests) of
        false -> false;
        true -> save_doc(Settings, AcctDb, SaveDoc, Type)
    end.

save_doc(Settings, AcctDb, SaveDoc, Type) ->
    Server = wh_json:get_value(<<"server_uri">>, Settings),

    {Meta, Binary} = knapsack_util:fetch_file_to_store(AcctDb, SaveDoc, Type),

    ok.

    
