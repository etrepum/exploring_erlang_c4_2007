-module(c4_adder_otp).
-behaviour(gen_server).
-export([start/0, add/2, stop/1]).
-export([init/1, terminate/2,
         handle_cast/2, handle_call/3]).

start() ->
    gen_server:start_link(?MODULE, 0, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

add(N, Pid) ->
    gen_server:call(Pid, {add, N}).

init(Count) ->
    {ok, Count}.

terminate(_Reason, _Count) ->
    ok.

handle_cast(stop, Count) ->
    {stop, normal, Count}.

handle_call({add, N}, _From, Count) ->
    {reply, N + Count, N + Count}.
