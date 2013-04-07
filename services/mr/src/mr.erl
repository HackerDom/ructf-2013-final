-module(mr).
-export([exec/2]).

-define(QUEUE_SIZE, 32).
-define(TIMEOUT, 3000).

exec(Module, Data) ->
    Mapped = parallel_exec(fun Module:map/3, Data),
    parallel_exec(fun Module:map/3, Mapped).

parallel_exec(Fun, Data) ->
    parallel_exec(Fun, Data, 0, orddict:new()).

parallel_exec(_Fun, [], 0, Collected)  ->
    orddict:to_list(Collected);
parallel_exec(Fun, [Data|Tail], WorkersCnt, Collected) when WorkersCnt < ?QUEUE_SIZE ->
    spawn_link(fun() -> worker(self(), Fun, Data) end),
    parallel_exec(Fun, Tail, WorkersCnt + 1, Collected);
parallel_exec(Fun, Data, WorkersCnt, Collected)  ->
    receive 
        {Key, Value} -> parallel_exec(Fun, Data, WorkersCnt, orddict:append(Key, Value, Collected));
        {'EXIT', _, normal} -> parallel_exec(Fun, Data, WorkersCnt - 1, Collected)
    after ?TIMEOUT -> erlang:error(too_hard)
    end.

worker(MasterPid, Fun, {Key, Value}) ->
    Fun(Key, Value, fun(EmitKey, EmitVal) -> MasterPid ! {EmitKey, EmitVal} end).
