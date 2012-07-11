%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Access module for the #sip_req{} record
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(smoke_sip_req).

-export([new/0
         ,socket/1, set_socket/2
         ,transport/1, set_transport/2
         ,method/1, set_method/2
         ,request_uri/1, set_request_uri/2
         ,request_body/1, set_request_body/2
         ,version/1
         ,sender_ip/1, set_sender_ip/2
         ,sender_port/1, set_sender_port/2
         ,onrequest/1, set_onrequest/2
         ,onresponse/1, set_onresponse/2
         ,headers/1, set_headers/2
         ,header/2, set_header/2
         ,reply/2
         ,pp_uri/1, pp_uri/2
        ]).

-export_type([sip_req/0]).

-include("smoke.hrl").
-include("smoke_sip.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec new/0 :: () -> sip_req().
new() -> #sip_req{}.

-spec socket/1 :: (sip_req()) -> inet:socket().
-spec set_socket/2 :: (sip_req(), inet:socket()) -> sip_req().
socket(#sip_req{socket=S}) -> S.
set_socket(#sip_req{}=Req, S) when is_port(S) ->
    Req#sip_req{socket=S}.

-spec transport/1 :: (sip_req()) -> module().
-spec set_transport/2 :: (sip_req(), module()) -> sip_req().
transport(#sip_req{transport=T}) -> T.
set_transport(#sip_req{}=Req, T) when is_atom(T) ->
    Req#sip_req{transport=T}.

-spec method/1 :: (sip_req()) -> sip_method().
-spec set_method/2 :: (sip_req(), sip_method() | ne_binary()) -> sip_req().
method(#sip_req{method=M}) -> M.

set_method(#sip_req{}=Req, ?NE_BINARY = M) ->
    set_method(Req,  wh_util:to_atom(M));
set_method(#sip_req{}=Req, M) when is_atom(M) ->
    true = lists:member(M, ?METHODS_SUPPORTED),
    Req#sip_req{method=M}.

-spec request_uri/1 :: (sip_req()) -> sip_uri().
-spec set_request_uri/2 :: (sip_req(), sip_uri() | ne_binary()) -> sip_req().
request_uri(#sip_req{request_uri=RU}) ->
    RU.
set_request_uri(#sip_req{}=Req, #sip_uri{}=RU) ->
    Req#sip_req{request_uri=RU};
set_request_uri(#sip_req{}=Req, ?NE_BINARY = RU) ->
    {#sip_uri{}=Uri, _} = smoke_sip:extract_uri(RU),
    Req#sip_req{request_uri=Uri}.

-spec request_body/1 :: (sip_req()) -> binary().
-spec set_request_body/2 :: (sip_req(), binary()) -> sip_req().
request_body(#sip_req{body=B}) -> B.
set_request_body(#sip_req{}=Req, B) -> Req#sip_req{body=B}.

-spec version/1 :: (sip_req()) -> sip_version().
version(#sip_req{version=V}) ->
    V.

-spec sender_ip/1 :: (sip_req()) -> inet:ip_address().
-spec set_sender_ip/2 :: (sip_req(), inet:ip_address()) -> sip_req().
sender_ip(#sip_req{sender_ip=IP}) -> IP.
set_sender_ip(#sip_req{}=Req, IP) ->
    Req#sip_req{sender_ip=IP}.

-spec sender_port/1 :: (sip_req()) -> inet:port_number().
-spec set_sender_port/2 :: (sip_req(), inet:port_number()) -> sip_req().
sender_port(#sip_req{sender_port=P}) -> P.
set_sender_port(#sip_req{}=Req, P) when is_integer(P) ->
    Req#sip_req{sender_port=P}.

-spec onrequest/1 :: (sip_req()) -> onrequest().
-spec set_onrequest/2 :: (sip_req(), onrequest()) -> sip_req().
onrequest(#sip_req{onrequest=OR}) -> OR.
set_onrequest(#sip_req{}=Req, OR) when is_function(OR) ->
    Req#sip_req{onrequest=OR}.

-spec onresponse/1 :: (sip_req()) -> onresponse().
-spec set_onresponse/2 :: (sip_req(), onresponse()) -> sip_req().
onresponse(#sip_req{onresponse=OR}) -> OR.
set_onresponse(#sip_req{}=Req, OR) when is_function(OR) ->
    Req#sip_req{onresponse=OR}.

-spec header/2 :: (sip_req(), ne_binary()) -> ne_binary() | 'undefined'.
header(#sip_req{headers=Hs}, K) -> props:get_value(K, Hs).
-spec set_header/2 :: (sip_req(), {sip_header() | ne_binary(), any()}) -> sip_req().
set_header(#sip_req{headers=Hs}=Req, {_K, _V}=KV) ->
    Req#sip_req{headers=[KV | Hs]}.

-spec headers/1 :: (sip_req()) -> sip_headers().
headers(#sip_req{headers=Hs}) -> Hs.
-spec set_headers/2 :: (sip_req(), sip_headers()) -> sip_req().
set_headers(#sip_req{}=Req, Hs) ->
    Req#sip_req{headers=Hs}.

-spec reply/2 :: (sip_req(), sip_response_code()) -> any().
reply(#sip_req{}=_Req, _Code) ->
    %% write reply to the socket
    ok.

-spec pp_uri/1 :: (sip_uri()) -> iolist().
-spec pp_uri/2 :: (sip_uri(), 'iolist' | 'binary') -> iolist() | binary().
pp_uri(#sip_uri{}=Uri) ->
    pp_uri(Uri, iolist).

pp_uri(#sip_uri{display_name=DN
                ,scheme=S
                ,user=U
                ,password=Pass
                ,host=H
                ,port=Port
                ,params=Params
                ,headers=Hdrs
               }
       ,Format)
  when Format =:= iolist orelse Format =:= binary ->
    Io = [ maybe_pp(DN, undefined, <<" <">>)
           ,wh_util:to_binary(S), <<":">>
           ,maybe_pp(U)
           ,maybe_pp(Pass, <<":">>)
           ,maybe_pp(H, <<"@">>)
           ,maybe_pp(Port, <<":">>)
           ,maybe_pp(DN =/= undefined, <<">">>)
           ,maybe_pp(Params)
           ,case Hdrs of
                [] -> <<>>;
                [{K,V}|Rest] ->
                    [
                     maybe_pp(true, <<"?">>)
                     ,maybe_pp_kv(K, V)
                     ,[maybe_pp_kv(Key, Val, <<"&">>) || {Key, Val} <- Rest]
                    ]
            end
         ],
    pp(Io, Format).

-spec pp/2 :: (Io, 'iolist' | 'binary') -> Io | binary().
pp(Io, iolist) -> Io;
pp(Io, binary) -> iolist_to_binary(Io).

%% conditionally print prefixes and suffixes based on the input's defined-edness
-spec maybe_pp/1 :: ('undefined' | binary()) -> binary().
-spec maybe_pp/2 :: ('undefined' | binary(), 'undefined' | binary()) -> binary().
-spec maybe_pp/3 :: (term(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) -> iolist().
maybe_pp(undefined, _, _) -> <<>>;
maybe_pp(V, Prefix, Suffix) -> [maybe_pp(Prefix), wh_util:to_binary(V), maybe_pp(Suffix)].

maybe_pp(undefined, _) -> <<>>;
maybe_pp(true, ?NE_BINARY = V) -> V;
maybe_pp(false, _) -> <<>>;
maybe_pp(?NE_BINARY = V, Prefix) -> [maybe_pp(Prefix), V];
maybe_pp(L, Sep) when is_list(L) ->
    [ maybe_pp_kv(K, V, Sep) || {K, V} <- L];
maybe_pp(V, Prefix) -> [maybe_pp(Prefix), wh_util:to_binary(V)].

maybe_pp(true) -> <<>>;
maybe_pp(false) -> <<>>;
maybe_pp(undefined) -> <<>>;
maybe_pp(V) when is_binary(V) -> V;
maybe_pp(L) when is_list(L) ->
    maybe_pp(L, <<";">>);
maybe_pp(V) -> wh_util:to_binary(V).

maybe_pp_kv(K, true) ->
    maybe_pp(K);
maybe_pp_kv(K, V) ->
    [maybe_pp(K), maybe_pp(V, <<"=">>)].

maybe_pp_kv(K, true, Prefix) ->
    maybe_pp(K, Prefix);
maybe_pp_kv(K, V, Prefix) ->
    [maybe_pp(K, Prefix, <<"=">>), maybe_pp(V)].

-ifdef(TEST).

pp_sip_uri_test() ->
    PPUri = <<"\"\" <sip:0000000000@192.168.1.1>;rport;tag=3et3X2avH64Xr">>,

    Params = [{<<"rport">>, true}
              ,{<<"tag">>, <<"3et3X2avH64Xr">>}
             ],

    URI = #sip_uri{
      display_name = <<"\"\"">>
      ,scheme = 'sip'
      ,user = <<"0000000000">>
      ,host = <<"192.168.1.1">>
      ,params=Params
      ,headers=[]
     },

    ?assertEqual(PPUri, pp_uri(URI, binary)).

pp_sip_uri_with_headers_test() ->
    PPUri = <<"sips:alice:pass@atlanta.com;maddr=239.255.255.1;ttl=15;method=INVITE?day=tuesday">>,
    Params = [{<<"maddr">>, <<"239.255.255.1">>}
              ,{<<"ttl">>, 15}
              ,{<<"method">>, 'INVITE'}
             ],

    URI = #sip_uri{
      display_name = 'undefined'
      ,scheme = 'sips'
      ,user = <<"alice">>
      ,password = <<"pass">>
      ,host = <<"atlanta.com">>
      ,params=Params
      ,headers=[{<<"day">>, <<"tuesday">>}]
     },

    ?assertEqual(PPUri, pp_uri(URI, binary)).

-endif.
