-module(web_appmod).
-export([out/4, return_error/2, return_ok/1]).

-include("../deps/yaws/include/yaws_api.hrl").

out(["upload"], 'POST', User, Arg) ->
    ok = upload(User, yaws_api:postvar(Arg, "name"), yaws_api:postvar(Arg, "code")),
    ok;
out(["exec"], 'POST', User, Arg) ->
    {ok, Data} = code_tools:parse_data(yaws_api:postvar(Arg, "data")),
    {ok, Result} = exec(User, yaws_api:postvar(Arg, "name"), Data),
    {ok, Result};
out(_Path, _Method, _User, _Json) ->
    throw(nopage).

exec(_User, undefined, _Code) ->
    throw(noname);
exec(_User, _Name, undefined) ->
    throw(nocode);
exec(User, Name, Code) ->
    appmod:exec(User, Name, Code).

upload(_User, undefined, _Code) ->
    throw(noname);
upload(_User, _Name, undefined) ->
    throw(nocode);
upload(User, Name, Code) ->
    appmod:upload(User, Name, Code).

return_error(Code, Msg) ->
    return_html(io_lib:format("ERROR: code: ~b message: ~s", [Code, Msg])).

return_ok(ok) ->
    return_html("All ok.");
return_ok({ok, Result}) ->
    return_html(io_lib:format("Result: ~w", [Result])).

return_html(Html) ->
    {html, lists:flatten(Html)}.
