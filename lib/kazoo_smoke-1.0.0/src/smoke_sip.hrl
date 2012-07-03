-ifndef(SMOKE_SIP_HRL).

-record(sip_uri_params, {
          transport = 'undefined' :: sip_transports() | 'undefined'
          ,maddr = 'undefined' :: ne_binary() | 'undefined' % overrides address derived from Host field
          ,ttl = 'undefined' :: non_neg_integer() | 'undefined'
          ,user = 'undefined' :: ne_binary() | 'undefined'
          ,method = 'undefined' :: 'undefined' | method()
          ,lr = 'undefined' :: 'undefined' | ne_binary()
         }).

-record(sip_uri, {
          scheme = <<"sip">> :: ne_binary()
         ,user = <<"nouser">> :: ne_binary()
         ,pass = <<>> :: binary()
         ,host = <<"nohost">> :: ne_binary()
         ,port :: integer()
         ,params :: sip_uri_params() 
         ,headers = [] :: wh_proplist()
         }).

-record(sip_req, {
          %% Transport
          socket :: inet:socket()
         ,transport :: module()
         ,connection = keepalive :: keepalive | close

         ,sender_ip :: inet:ip_address()
         ,sender_port :: inet:port_number()

          %% Makes up the first line
         ,method = 'INVITE' :: wh_sip:method()
         ,request_uri :: sip_uri() % Request-URI
         ,version = wh_sip:version() :: wh_sip:version()

          %% Request
         ,headers = [] :: wh_sip:headers()
         ,p_headers = [] :: [any()] % processed headers
         ,onrequest

          %% Body
         ,body = <<>> :: binary()
         ,buffer = <<>> :: binary()

          %% Response
         ,onresponse
         }).

-opaque sip_req() :: #sip_req{}.
-opaque sip_uri() :: #sip_uri{}.
-opaque sip_uri_params() :: #sip_uri_params{}.

-type method() ::
        'INVITE'    | %[RFC3261]
        'ACK'       | %[RFC3261]
        'BYE'       | %[RFC3261]
        'CANCEL'    | %[RFC3261]
        'REGISTER'  | %[RFC3261]
        'OPTIONS'   | %[RFC3261]
        'SUBSCRIBE' | %[RFC3265]
        'NOTIFY'    | %[RFC3265]
        'UPDATE'    | %[RFC3311]
        'MESSAGE'   | %[RFC3428]
        'REFER'     | %[RFC3515]
        'INFO'.       %[RFC2976]
-define(METHODS_SUPPORTED, ['INVITE','ACK','BYE','CANCEL','REGISTER','OPTIONS'
                            ,'SUBSCRIBE','NOTIFY','UPDATE','MESSAGE','REFER'
                            ,'INFO'
                           ]).

-type sip_transports() :: 'udp' | 'tcp' | 'tls' | 'sctp'.

-type version() :: 'SIP/2.0'.

-type header() :: 'Via' | 'To' | 'From' | 'CSeq' | 'Call-ID' | % these go in all responses
                  'Max-Forwards' | 'Contact' | 'Content-Type' |
                  'Content-Length' |
                  %% Optional Headers
                  'Accept' | 'Accept-Encoding' | 'Accept-Language' |
                  'Alert-Info' | 'Allow' | 'Authentication-Info' |
                  'Authorization' | 'Call-Info' | 'Content-Disposition' |
                  'Content-Encoding' | 'Content-Language' | 'Date' |
                  'Error-Info' | 'Expires' | 'In-Reply-To' | 'Min-Expires' |
                  'MIME-Version' | 'Organization' | 'Priority' |
                  'Proxy-Authenticate' | 'Proxy-Authorization' | 'Proxy-Require' |
                  'Record-Route' | 'Reply-To' | 'Require' | 'Retry-After' |
                  'Route' | 'Server' | 'Subject' | 'Supported' | 'Timestamp' |
                  'Unsupported' | 'User-Agent' | 'Warning' | 'WWW-Authenticate'.

-type headers() :: [{header(), iolist()}].

-type status() :: non_neg_integer() | binary().

-define(SMOKE_SIP_HRL, true).
-endif.
