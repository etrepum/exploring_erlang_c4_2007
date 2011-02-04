-module(c4_http).
-export([start/0, loop/2, stop/0]).
-define(DEFAULTS, [{name, ?MODULE},
                   {port, 9952}]).

start() ->
    DocRoot = filename:dirname(filename:dirname(code:which(?MODULE))),
    code:add_patha(filename:join([DocRoot, "mochiweb-c4", "ebin"])),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    {ok, Pid} = c4_adder_otp:start(),
    register(c4_hit_counter, Pid),
    mochiweb_http:start([{loop, Loop} | ?DEFAULTS]).

stop() ->
    c4_adder_otp:stop(c4_hit_counter),
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    Hits = c4_adder_otp:add(1, c4_hit_counter),
    case Req:get(method) of
        M when M =:= 'GET'; M =:= 'HEAD' ->
            case Path of
                "timer" ->
                    Response = Req:ok({"text/plain", chunked}),
                    timer(Response);
                "static" ->
                    Req:ok({"text/plain", "static response"});
                "hits" ->
                    Req:ok({"text/plain",
                            io_lib:format("Hits: ~p~n", [Hits])});
                "nodes" ->
                    Req:ok({"text/plain",
                            io_lib:format("~p~n", [nodes()])});
                "dump" ->
                    Req:ok({"text/plain",
                            io_lib:format("~p~n", [Req:dump()])});
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        _ ->
            Req:respond({501, [], ""})
    end.

timer(Req) ->
    Req:write_chunk(io_lib:format("The time is: ~p~n",
                                  [calendar:local_time()])),
    timer:sleep(1000),
    timer(Req).
