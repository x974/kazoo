%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(milliwatt_handlers).

-export([handle_route_req/2]).

-include("milliwatt.hrl").

-spec handle_route_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_route_req(JObj, Props) ->
    lager:debug("route_req received for milliwatt monitoring"),

    CallerId = wh_json:get_value[]([<<"Custom-Channel-Vars">>, <<"Caller-ID">>], JObj, undefined),      
    ActionC = whapps_config:get(<<"milliwatt">>, [<<"caller_id">>, CallerId]),
    case ActionC of
        <<"echo">> -> echo_test(JObj); 
        <<"tone">> -> tone_test(JObj)
    end

    To = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"To">>], JObj, undefined),
    ActionT = whapps_config:get(<<"milliwatt">>, [<<"to">>, To]),
    case ActionT of
        <<"echo">> -> echo_test(JObj);
        <<"tone">> -> tone_test(JObj)
    end

       
    Q = props:get_value(queue, Props),
    whapps_call:route_response(JObj, Q, park).

-spec echo_test/1 :: (wh_josn:json_object()) -> any().
echo_test(JObj) ->
    ok.

-spec tone_test/1 :: (wh_json:json_object()) -> any().
    ok.

