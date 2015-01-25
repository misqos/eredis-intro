-module(example).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([get/1, set/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, State} = eredis:start_link(),
    {ok, State}.

handle_call({get, Key}, _From, State) ->
    Response = eredis:q(State, [ "GET", Key ]),
    {reply, Response, State};

handle_call({set, Key, Value}, _From, State) ->
    Response = eredis:q(State, [ "SET", Key, Value ]),
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).
