map(_Key, Value, Emit) ->
    [Emit(Col, V) || {Col, V} <- lists:zip(lists:seq(1, length(Value), Value))].
reduce(Key, Values, Emit) ->
    MapFun = fun(Val, Acc) -> dict:update_counter(Val, 1, Acc) end,
    Freqs = lists:foldl(MapFun, dict:new(), Values),
    {Letter, _} = dict:foldl(fun dict_max/2, {"", 0}, Freqs),
    ok = check(Letter, Key),
    Emit(Key, ok).

dict_max({Key, Value}, {_MaxKey, MaxValue}) when Value > MaxValue -> 
    {Key, Value};
dict_max({Key, Value}, _) ->
    {Key, Value}.

check('a', 3) -> ok;
check('b', 1) -> ok;
check('c', 2) -> ok;
check('d', 4) -> ok.
