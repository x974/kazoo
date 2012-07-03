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
         ,version/1
         ,sender_ip/1, set_sender_ip/2
         ,sender_port/1, set_sender_port/2
         ,onrequest/1, set_onrequest/2
         ,reply/2
        ]).

-export_type([sip_req/0]).

-include("smoke.hrl").
-include("smoke_sip.hrl").

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

-spec reply/2 :: (sip_req(), sip_response_code()) -> any().
reply(#sip_req{}=_Req, _Code) ->
    %% write reply to the socket
    ok.
