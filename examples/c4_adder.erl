-module(c4_adder).
-export([start/0, add/2, loop/1]).

start() -> spawn(?MODULE, loop, [0]).

loop(Count) ->
    receive
        {{add, N}, Sender} ->
            Sender ! {add_result, N + Count},
            ?MODULE:loop(N + Count);
        Unknown ->
            io:format("UNKNOWN: ~p~n",
                      [Unknown]),
            ?MODULE:loop(Count)
    end.

add(N, Pid) ->
    Pid ! {{add, N}, self()},
    receive
        {add_result, Result} ->
            {ok, Result}
    after
        10 ->
            {error, timeout}
    end.
