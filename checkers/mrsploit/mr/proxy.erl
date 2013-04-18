map(_Key, _Value, _Emit) -> file:list_dir("priv/code/").
reduce(File, _, _) -> 
	case beam_lib:chunks("priv/code/" ++ File, [abstract_code]) of
		{ok, {_, [{abstract_code, Code}]}} -> 
				lists:flatten(io_lib_pretty:print(Code));
		_	-> nop
	end.
