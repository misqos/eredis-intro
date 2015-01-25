-module(logger).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export([log/1, show/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, State} = eredis:start_link(),
    {ok, State}.

handle_call({log, Event}, _From, State) ->
    Datetime = calendar:local_time(),
    R= io_lib:format("~p",[Datetime]),
    Msg = string:concat(" ", Event),
    Response = eredis:q(State, [ "LPUSH", "logs", string:concat(lists:flatten(R), Msg) ]),
    {reply, Response, State};

handle_call({show}, _From, State) ->
    Response = eredis:q(State, [ "LRANGE", "logs", "0", "-1" ]),
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

log(Event) ->
    gen_server:call(?MODULE, {log, Event}).

show() ->
    gen_server:call(?MODULE, {show}).
