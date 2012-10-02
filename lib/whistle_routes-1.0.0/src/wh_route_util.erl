%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_route_util).

-export([itsp_ips/0]).
-export([kazoo_ips/0]).
-export([available_kazoo_routes/0]).
-export([numbers_routes/0]).

-export([discover_fs_nodes/0]).
-export([discover_itsp_ips/0]).

-include_lib("whistle_routes/src/wh_routes.hrl").

-spec itsp_ips/0 :: () -> proplist().
itsp_ips() ->
    [{IP, <<"itsp">>} 
     || IP <- discover_itsp_ips()
    ].

-spec kazoo_ips/0 :: () -> proplist().
kazoo_ips() ->    
    [{IP, <<"kazoo">>} 
     || IP <- discover_fs_nodes()
    ].

-spec available_kazoo_routes/0 :: () -> proplist().
available_kazoo_routes() ->
    Available = wh_util:join_binary([<<"sip:", IP/binary>> 
                                         || IP <- discover_fs_nodes()
                                    ], <<";">>),
    [{<<"available_kazoo_routes">>, Available}].    

-spec numbers_routes/0 :: () -> proplist().
numbers_routes() ->
    [begin
         Dst = wh_util:join_binary([<<"sip:", Server/binary>> 
                                        || Server <- Servers
                                   ], <<";">>),
         {Number, Dst}
     end
     || {Number, Servers} <- wnm_routes:get_number_servers()
    ].

-spec discover_fs_nodes/0 :: () -> proplist().
discover_fs_nodes() ->
    case couch_mgr:open_doc(?WH_CONFIG_DB, <<"ecallmgr">>) of
        {error, _R} ->
            lager:debug("Unable to open ~s/ecallmgr: ~p", [?WH_CONFIG_DB, _R]),
            [];
        {ok, JObj} ->
            extract_ecallmgr_fs_nodes(JObj)
    end.

-spec extract_ecallmgr_fs_nodes/1 :: (wh_json:json_object()) -> proplist().
extract_ecallmgr_fs_nodes(JObj) ->
    lists:foldr(fun(K, I) ->
                        case wh_json:get_value([K, <<"advertised_ips">>], JObj, []) of
                            [] ->
                                Nodes = wh_json:get_value([K, <<"fs_nodes">>], JObj, []),
                                resolve_fs_nodes(Nodes, I);
                            IPs -> IPs ++ I
                        end     
                end, [], wh_json:get_keys(JObj)).

-spec resolve_fs_nodes/2 :: (ne_binary(), proplist()) -> proplist().
resolve_fs_nodes(Nodes, IPs) ->
    lists:foldr(fun(Node, I) -> 
                        [_, Domain] = binary:split(Node, <<"@">>),
                        resolve_ip(Domain, I)
                end, IPs, Nodes).

-spec discover_itsp_ips/0 :: () -> proplist().
discover_itsp_ips() ->
    Routines = [fun ecallmgr_trusted_ips/1
                ,fun offnet_resource_gateways/1
               ],
    lists:foldr(fun(F, I) -> F(I) end, [], Routines).

-spec offnet_resource_gateways/1 :: (proplist()) -> proplist().
offnet_resource_gateways(IPs) ->
    ViewOptions = [{include_docs, true}],
    case couch_mgr:get_results(<<"offnet">>, <<"resources/listing_active_by_weight">>, ViewOptions) of
        {error, _R} ->
            lager:debug("Unable to get view results for offnet resources: ~p", [_R]),
            IPs;
        {ok, JObjs} ->
            lists:foldr(fun(JObj, I) ->
                                Doc = wh_json:get_value(<<"doc">>, JObj),
                                resource_gateway_ips(Doc, I)
                        end, IPs, JObjs)
    end.

-spec resource_gateway_ips/2 :: (wh_json:json_object(), proplist()) -> proplist().
resource_gateway_ips(JObj, IPs) ->
    lists:foldr(fun(Gateway, I) ->
                        case wh_json:is_true(<<"enabled">>, Gateway) of
                            false -> I;
                            true ->
                                Server = wh_json:get_value(<<"server">>, Gateway),
                                resolve_ip(Server, I)
                        end
                end, IPs, wh_json:get_value(<<"gateways">>, JObj, [])).

-spec ecallmgr_trusted_ips/1 :: (proplist()) -> proplist().
ecallmgr_trusted_ips(IPs) ->
    case couch_mgr:open_doc(?WH_CONFIG_DB, <<"ecallmgr">>) of
        {error, _R} ->
            lager:debug("Unable to open ~s/ecallmgr: ~p", [?WH_CONFIG_DB, _R]),
            IPs;
        {ok, JObj} ->
            extract_ecallmgr_trusted_ips(JObj, IPs)
    end.

-spec extract_ecallmgr_trusted_ips/2 :: (wh_json:json_object(), proplist()) -> proplist().
extract_ecallmgr_trusted_ips(JObj, IPs) ->
    lists:foldr(fun(K, I) ->
                        case wh_json:get_value([K, <<"acls">>], JObj) of
                            undefined -> I;
                            ACLs -> extract_acls_trusted_ips(ACLs, I)
                        end
                end, IPs, wh_json:get_keys(JObj)).
                                     
-spec extract_acls_trusted_ips/2 :: (wh_json:json_object(), proplist()) -> proplist().
extract_acls_trusted_ips(ACLs, IPs) ->        
    lists:foldr(fun(K, I) ->
                        case wh_json:get_value([K, <<"network-list-name">>], ACLs) =:= <<"trusted">>
                            andalso wh_json:get_value([K, <<"type">>], ACLs) =:= <<"allow">>
                        of
                            false -> I;
                            true -> 
                                CIDR = wh_json:get_value([K, <<"cidr">>], ACLs), 
                                [expand_cidr_notation(CIDR)|I]
                        end
                end, IPs, wh_json:get_keys(ACLs)).

-spec expand_cidr_notation/1 :: (ne_binary()) -> ne_binary().
expand_cidr_notation(CIDR) ->
    %% TODO: this currently supports /32 only....
    case binary:split(CIDR, <<"/">>) of
        [IP, _] -> IP;
        _ -> CIDR
    end.

-spec resolve_ip/2 :: (ne_binary(), proplist()) -> proplist().
resolve_ip(Domain, IPs) ->
    Lookup = <<"_sip._udp.", Domain/binary>>,
    case inet_res:lookup(wh_util:to_list(Lookup), in, srv) of
        [] -> resolve_a_records([Domain], IPs);
        SRVs ->
            resolve_a_records([D || {_, _, _, D} <- SRVs], IPs)
    end.

-spec resolve_a_records/2 :: (ne_binary(), proplist()) -> proplist().
resolve_a_records(Domains, IPs) ->
    lists:foldr(fun(Domain, I) ->
                        case wh_util:is_ipv4(Domain) of
                            true -> [Domain|I];
                            false ->
                                D = wh_util:to_list(Domain),
                                resolve_a_record(D, I)
                        end
                end, IPs, Domains).

-spec resolve_a_record/2 :: (ne_binary(), proplist()) -> proplist().
resolve_a_record(Domain, IPs) ->
    lists:foldr(fun(IPTuple, I) ->
                        [iptuple_to_binary(IPTuple)|I]
                end, IPs, inet_res:lookup(Domain, in, a)).

iptuple_to_binary({A,B,C,D}) ->
    <<(wh_util:to_binary(A))/binary, "."
      ,(wh_util:to_binary(B))/binary, "."
      ,(wh_util:to_binary(C))/binary, "."
      ,(wh_util:to_binary(D))/binary>>.
