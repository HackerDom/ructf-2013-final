map(_Key, Value, Emit) ->
    [Emit(Col, V) || {Col, V} <- lists:zip(lists:seq(1, length(Value)), Value)].
reduce(Key, Values, Emit) ->
    MapFun = fun(Val, Acc) -> dict:update_counter(Val, 1, Acc) end,
    Freqs = lists:foldl(MapFun, dict:new(), Values),
    {Letter, _} = dict:fold(fun dict_max/3, {"", 0}, Freqs),
    Emit(integer_to_list(Key), check(binary_to_list(<<Letter>>), Key)).

dict_max(Key, Value, {_MaxKey, MaxValue}) when Value > MaxValue -> 
    {Key, Value};
dict_max(_, _, Max) ->
    Max.
