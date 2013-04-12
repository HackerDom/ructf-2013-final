-module(json_appmod).
-export([out/4, return_ok/1, return_error/2]).

-include("../deps/yaws/include/yaws_api.hrl").

out(Path, Method, User, Arg = #arg{})  ->
    {ok, Json} = json2:decode_string(binary_to_list(Arg#arg.clidata)),
    out(Path, Method, User, Json);
out(["upload"], 'POST', User, {struct, [{"name", Name}, {"code", ErlangCode}]}) ->
    ok = appmod:upload(User, Name, ErlangCode),
    ok;
out(["exec"], 'POST', User, {struct, [{"name", Name}, {"data", {struct, Data}}]}) ->
    {ok, Result} = appmod:exec(User, Name, Data),
    {ok, Result};
out(_Path, _Method, _User, _Json) ->
    throw(nopage).

return_error(Code, Msg) ->
    Json = {struct, [
                {"status", "FAIL"},
                {"error", {struct, [
                    {"code", Code},
                    {"str", Msg}
                ]}}
            ]},
    return_json(json2:encode(Json)).

return_ok(ok) ->
    Json = {struct, [ {"status", "OK"} ] },
    return_json(json2:encode(Json));
return_ok({ok, Result}) ->
    Json = {struct, [ {"status", "OK"}, {"result", Result} ] },
    return_json(json2:encode(Json)).

return_json(Json) ->
    {content,
    "application/json",
    Json}.
