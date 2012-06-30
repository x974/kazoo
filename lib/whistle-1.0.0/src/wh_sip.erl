%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% SIP-related helpers
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_sip).

-export([version/0]).

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

-export_type([method/0, version/0, header/0, headers/0, status/0]).

-spec version/0 :: () -> 'SIP/2.0'.
version() ->
    'SIP/2.0'.
