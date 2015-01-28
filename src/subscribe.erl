-module(subscribe).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export([subscribe/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, State} = eredis_sub:start_link(),
    {ok, State}.

handle_call({subscribe, Event}, _From, State) ->
    Receiver = spawn_link (fun () ->
        eredis_sub:controlling_process(State),
        eredis_sub:subscribe(State, [Event]),
        eredis_sub:receiver(State)
    end),
    {reply, Receiver, State};

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

subscribe(Event) ->
    gen_server:call(?MODULE, {subscribe, Event}).
