-record(sip_req, {
          %% Transport
          socket :: inet:socket()
         ,transport :: module()
         ,connection = keepalive :: keepalive | close

          %% Dialog
         ,pid :: pid()

          %% Makes up the first line
         ,method = 'INVITE' :: wh_sip:method()
         ,request_uri :: binary() % Request-URI
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
