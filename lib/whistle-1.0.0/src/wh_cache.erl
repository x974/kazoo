%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Simple cache server
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_cache).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([store/2, store/3, store/4]).
-export([peek/1]).
-export([fetch/1, fetch_keys/0]).
-export([erase/1]).
-export([flush/0]).
-export([filter/1]).

-export([start_local_link/0]).
-export([store_local/3, store_local/4, store_local/5]).
-export([peek_local/2]).
-export([fetch_local/2, fetch_keys_local/1]).
-export([erase_local/2]).
-export([flush_local/1]). 
-export([filter_local/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(SERVER, ?MODULE).
-define(EXPIRES, 3600). %% an hour
-define(EXPIRE_CHECK, 30000).

-record(state, {
          pb_pid :: pid() %% PB connection to Riak
         ,keys :: dict() %% list of keys and expiration
         ,bucket :: ne_binary() %% name of the cache proc
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [?MODULE], []).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

start_local_link() ->
    gen_server:start_link(?MODULE, [local], []).

%% T - seconds to store the pair
-spec store/2 :: (term(), term()) -> 'ok'.
-spec store/3 :: (term(), term(), pos_integer() | 'infinity' | function()) -> 'ok'.
-spec store/4 :: (term(), term(), pos_integer() | 'infinity', function()) -> 'ok'.

store(K, V) ->
    store(K, V, ?EXPIRES).

store(K, V, Fun) when is_function(Fun, 3) ->
    store(K, V, ?EXPIRES, Fun);
store(K, V, T) ->
    store_local(?SERVER, K, V, T).

store(K, V, T, Fun) when is_function(Fun, 3) ->
    store_local(?SERVER, K, V, T, Fun).

-spec peek/1 :: (term()) -> {'ok', term()} | {'error', 'not_found'}.
peek(K) ->
    peek_local(?SERVER, K).

-spec fetch/1 :: (term()) -> {'ok', term()} | {'error', 'not_found'}.
fetch(K) ->
    fetch_local(?SERVER, K).

-spec erase/1 :: (term()) -> 'ok'.
erase(K) ->
    erase_local(?SERVER, K).

-spec flush/0 :: () -> 'ok'.
flush() ->
    flush_local(?SERVER).

-spec fetch_keys/0 :: () -> [term(),...] | [].
fetch_keys() ->
    fetch_keys_local(?SERVER).

-spec filter/1 :: (fun((term(), term()) -> boolean())) -> proplist().
filter(Pred) when is_function(Pred, 2) ->
    filter_local(?SERVER, Pred).

%% Local cache API
-spec store_local/3 :: (pid() | ?SERVER, term(), term()) -> 'ok'.
-spec store_local/4 :: (pid() | ?SERVER, term(), term(), pos_integer() | 'infinity' | function() | {atom(), atom()}) -> 'ok'.
-spec store_local/5 :: (pid() | ?SERVER, term(), term(), pos_integer() | 'infinity', function() | {atom(), atom()}) -> 'ok'.

store_local(Srv, K, V) when is_pid(Srv) orelse Srv =:= ?SERVER ->
    store_local(Srv, K, V, ?EXPIRES).

store_local(Srv, K, V, Fun) when is_pid(Srv) orelse Srv =:= ?SERVER, is_function(Fun, 3) ->
    store_local(Srv, K, V, ?EXPIRES, Fun);
store_local(Srv, K, V, T) when is_pid(Srv) orelse Srv =:= ?SERVER ->
    gen_server:cast(Srv, {store, encode(K), encode(V), T, undefined}).

store_local(Srv, K, V, T, Fun) when is_pid(Srv) orelse Srv =:= ?SERVER, is_function(Fun, 3) ->
    gen_server:cast(Srv, {store, encode(K), encode(V), T, Fun}).

-spec peek_local/2 :: (pid() | ?SERVER, term()) -> {'ok', term()} | {'error', 'not_found'}.
peek_local(Srv, K) when is_pid(Srv) orelse Srv =:= ?SERVER ->
    case gen_server:call(Srv, {peek, encode(K)}) of
        {ok, V} -> {ok, decode(V)};
        E -> E
    end.

-spec fetch_local/2 :: (pid() | ?SERVER, term()) -> {'ok', term()} | {'error', 'not_found'}.
fetch_local(Srv, K) when is_pid(Srv) orelse Srv =:= ?SERVER ->
    case gen_server:call(Srv, {fetch, encode(K)}) of
        {ok, V} -> {ok, decode(V)};
        E -> E
    end.

-spec erase_local/2 :: (pid() | ?SERVER, term()) -> 'ok'.
erase_local(Srv, K) when is_pid(Srv) orelse Srv =:= ?SERVER ->
    gen_server:cast(Srv, {erase, encode(K)}).

-spec flush_local/1 :: (pid() | ?SERVER) -> 'ok'.
flush_local(Srv) when is_pid(Srv) orelse Srv =:= ?SERVER ->
    gen_server:cast(Srv, {flush}).

-spec fetch_keys_local/1 :: (pid() | ?SERVER) -> [term(),...] | [].
fetch_keys_local(Srv) when is_pid(Srv) orelse Srv =:= ?SERVER ->
    case gen_server:call(Srv, fetch_keys) of
        L when is_list(L) -> [decode(K) || K <- L];
        E -> E
    end.

-spec filter_local/2 :: (pid() | ?SERVER, fun((term(), term()) -> boolean())) -> proplist().
filter_local(Srv, Pred) when (is_pid(Srv) orelse Srv =:= ?SERVER) andalso is_function(Pred, 2) ->
    gen_server:call(Srv, {filter, Pred}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name]) ->
    put(callid, ?LOG_SYSTEM_ID),
    _ = erlang:send_after(?EXPIRE_CHECK, self(), flush),
    lager:debug("started new cache proc: ~s", [Name]),

    RiakNode = "db001-qa-vb.2600hz.com",
    RiakPort = 8087,
    lager:debug("connecting to ~s:~b", [RiakNode, RiakPort]),

    {ok, Pid} = riakc_pb_socket:start_link(RiakNode, RiakPort),

    {ok, #state{bucket=wh_util:to_binary(Name), keys=dict:new(), pb_pid=Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({peek, K}, _, #state{keys=Dict, pb_pid=Pid, bucket=Bucket}=State) ->
    case dict:find(K, Dict) of
        {ok, {_, _, _, _}} ->
            case riakc_pb_socket:get(Pid, Bucket, K) of
                {ok, O} -> {reply, {ok, riakc_obj:get_value(O)}, State, hibernate};
                _ -> {reply, {error, not_found}, State}
            end;
        error -> {reply, {error, not_found}, State}
    end;
handle_call({fetch, K}, _, #state{keys=Dict, pb_pid=Pid, bucket=Bucket}=State) ->
    case dict:find(K, Dict) of
        {ok, {infinity=T, _, T, _}} ->
            case riakc_pb_socket:get(Pid, Bucket, K) of
                {ok, O} -> {reply, {ok, riakc_obj:get_value(O)}, State, hibernate};
                _ -> {reply, {error, not_found}, State}
            end;
        {ok, {_, _, T, F}} ->
            case riakc_pb_socket:get(Pid, Bucket, K) of
                {ok, O} -> {reply
                            ,{ok, riakc_obj:get_value(O)}
                            ,State#state{keys=dict:update(K, fun(_) ->
                                                                     {wh_util:current_tstamp()+T, ok, T, F} 
                                                             end, Dict)
                                        }
                            ,hibernate
                           };
                _ -> {reply, {error, not_found}, State}
            end;
        error -> {reply, {error, not_found}, State}
    end;
handle_call(fetch_keys, _, #state{keys=Dict}=State) ->
    {reply, dict:fetch_keys(Dict), State};

handle_call({filter, _Pred}, _, State) ->
    {reply, {error, not_implemented}, State}.
    %% KV = dict:map(fun(_, {_,V,_, _}) -> V end, Dict),
    %% {reply, dict:to_list(dict:filter(Pred, KV)), Dict}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({store, K, V, infinity=T, F}, #state{keys=Dict, pb_pid=Pid, bucket=Bucket}=State) ->
    Oa = riakc_obj:new(Bucket, K, V),
    riakc_pb_socket:put(Pid, Oa, []),
    {noreply, State#state{keys=dict:store(K, {T, ok, T, F}, Dict)}, hibernate};

handle_cast({store, K, V, T, F}, #state{keys=Dict, pb_pid=Pid, bucket=Bucket}=State) ->
    Oa = riakc_obj:new(Bucket, K, V),
    riakc_pb_socket:put(Pid, Oa, []),
    {noreply, State#state{keys=dict:store(K, {wh_util:current_tstamp()+T, ok, T, F}, Dict)}, hibernate};

handle_cast({erase, K}, #state{keys=Dict, pb_pid=Pid, bucket=Bucket}=State) ->
    ok = case dict:find(K, Dict) of
             {ok, {_, _, _, undefined}} -> ok;
             {ok, {_, _, _, F}} ->
                 spawn(fun() ->
                               {ok, Oa} = riakc_pb_socket:get(Pid, Bucket, K),
                               _ = riakc_pb_socket:delete(Pid, Bucket, K),
                               F(decode(K), decode(riakc_obj:get_value(Oa)), erase)
                       end),
                 ok;
             _ -> ok
         end,
    {noreply, State#state{keys=dict:erase(K, Dict)}, hibernate};

handle_cast({flush}, #state{keys=Dict, pb_pid=Pid, bucket=Bucket}=State) ->
    _ = [(fun({_, {_, _, _, undefined}}) -> ok;
             ({K, {_, _, _, F}}) ->
                  spawn(fun() ->
                                {ok, Oa} = riakc_pb_socket:get(Pid, Bucket, K),
                                _ = riakc_pb_socket:delete(Pid, Bucket, K),
                                F(decode(K), decode(riakc_obj:get_value(Oa)), flush)
                        end)
          end)(Elem)
         || Elem <- dict:to_list(Dict) 
        ],
    {noreply, State#state{keys=dict:new()}, hibernate}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(flush, #state{keys=Dict, pb_pid=Pid, bucket=Bucket}=State) ->
    Now = wh_util:current_tstamp(),
    Filter = fun(_, {infinity, _, _, _}) -> true;
                (_, {T, _, _, _}) when Now < T -> true;
                (_, {_, _, _, undefined}) -> false;
                (K, {_, _, _, F}) ->
                     spawn(fun() ->
                                   {ok, Oa} = riakc_pb_socket:get(Pid, Bucket, K),
                                   _ = riakc_pb_socket:delete(Pid, Bucket, K),
                                   F(decode(K), decode(riakc_obj:get_value(Oa)), expire)
                           end),
                     false
             end,
    _ = erlang:send_after(?EXPIRE_CHECK, self(), flush),
    {noreply, State#state{keys=dict:filter(Filter, Dict)}, hibernate};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
encode(V) ->
    term_to_binary(V, [compressed]).
decode(V) ->
    binary_to_term(V).
