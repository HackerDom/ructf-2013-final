map(_Key, Value, Emit) ->
    Emit(Value, 1).
reduce(Key, Values, Emit) ->
    S = lists:sum(Values),
    Emit(Key, integer_to_list(S)).
