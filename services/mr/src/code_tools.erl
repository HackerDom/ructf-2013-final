-module(code_tools).
-export([compile/2, check_security/1]).

compile(ModuleName, ErlangCode) when is_binary(ErlangCode) ->
    compile(ModuleName, binary_to_list(ErlangCode));
compile(ModuleName, ErlangCode) ->
    ModuleHeader = "-module(" ++ ModuleName ++ ").",
    ExportHeader = "-export([map/3, reduce/3]).",
    ModuleCode = string:join([ModuleHeader, ExportHeader, ErlangCode], "\n"),
    {ok, Tokens, _} = erl_scan:string(ModuleCode),
    FormTokens = split_forms(Tokens),
    Forms  = lists:map(fun(T) -> {ok, Form} = erl_parse:parse_form(T), Form end, FormTokens),
    {ok, _, Beam} = compile:forms(Forms),
    {ok, Beam}.

split_forms(Tokens) ->
    split_forms(Tokens, [], []). 
split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([{dot, N} | Tokens], CurrentToken, Acc) ->
    Token = lists:reverse([{dot, N} | CurrentToken]),
    split_forms(Tokens, [], [Token | Acc]);
split_forms([Token | Tokens], CurrentToken, Acc) ->
    split_forms(Tokens, [Token | CurrentToken], Acc).

check_security(ModuleName) ->
    {ok, {ModuleName, [{imports, Imports}]}} = beam_lib:chunks(ModuleName, [imports]),
    WhiteList = whitelist(),
    check_security(Imports, WhiteList).

check_security([], _) ->
    ok;
check_security([Import | Tail], WhiteList) ->
    ok =  match(Import, WhiteList),
    check_security(Tail, WhiteList).

match(_, []) ->
    {error, secfail};
match({M, F, _}, [{M, F} | _]) ->
    ok;
match({M, _, _}, [{M, "_"} | _]) ->
    ok;
match(I, [_ | T]) ->
    match(I, T).

whitelist() ->
    [{calendar, "_"},
     {dict, "_"},
     {erlang, atom_to_list},
     {erlang, binary_to_list},
     {erlang, integer_to_list},
     {erlang, float_to_list},
     {erlang, tuple_to_list},
     {erlang, list_to_binary},
     {erlang, list_to_integer},
     {erlang, list_to_tuple},
     {erlang, list_to_existing_atom},
     {erlang, setelement},
     {erlang, element},
     {erlang, size},
     {erlang, split},
     {erlang, error},
     {erlang, throw},
     {erlang, time},
     {erlang, date},
     {erlang, now},
     {erlang, universaltime},
     {erlang, localtime},
     {erlang, localtime_to_universaltime},
     {erlang, round},
     {erlang, atom_to_list},
     {erlang, binary_to_list},
     {erlang, integer_to_list},
     {erlang, float_to_list},
     {erlang, tuple_to_list},
     {erlang, list_to_binary},
     {erlang, list_to_integer},
     {erlang, list_to_tuple},
     {erlang, list_to_existing_atom},
     {erlang, setelement},
     {erlang, element},
     {erlang, size},
     {erlang, split},
     {erlang, error},
     {erlang, throw},
     {erlang, time},
     {erlang, date},
     {erlang, now},
     {erlang, universaltime},
     {erlang, localtime},
     {erlang, localtime_to_universaltime},
     {gb_sets,	"_"},
     {gb_trees,	"_"},
     {io_lib,	"_"},
     {lists,	"_"},
     {math,	"_"},
     {orddict,	"_"},
     {ordsets,	"_"},
     {proplists,	"_"},
     {queue,	"_"},
     {re,	"_"},
     {sets,	"_"},
     {sofs,	"_"},
     {string,	"_"}].
