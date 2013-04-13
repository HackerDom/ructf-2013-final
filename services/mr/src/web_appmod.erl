-module(web_appmod).
-export([out/4, return_error/2, return_ok/1]).

-include("../deps/yaws/include/yaws_api.hrl").

out(["index"], 'GET', _User, _Arg) ->
    {ok, index_dtl, []};
out(["examples"], 'GET', _User, _Arg) ->
    {ok, examples_dtl, []};
out(["upload"], 'GET', _User, _Arg) ->
    {ok, upload_dtl, [{method, get}, {content, ""}]};
out(["exec"], 'GET', _User, _Arg) ->
    {ok, exec_dtl, [{method, get}, {content, ""}]};
out(["upload"], 'POST', User, Arg) ->
    {ok, Name} = yaws_api:postvar(Arg, "name"), 
    {ok, Code} = yaws_api:postvar(Arg, "code"),
    ok = appmod:upload(User, Name, Code),
    {ok, upload_dtl, [{method, post}, {content, "Uploaded!"}]};
out(["exec"], 'POST', User, Arg) ->
    {ok, Data} = yaws_api:postvar(Arg, "data"),
    {ok, ErlData} = code_tools:parse_data(Data),
    {ok, Name} = yaws_api:postvar(Arg, "name"),
    Result = appmod:exec(User, Name, ErlData),
    {ok, exec_dtl, [{method, post}, {content, Result}]};
out(_Path, _Method, _User, _Json) ->
    throw(nopage).

return_error(Code, Msg) ->
    {ok, Output} = error_dtl:render([{code, Code}, {msg, Msg}]),
    {html, Output}.

return_ok({ok, Render, Params}) ->
    {ok, Output} = Render:render(Params),
    {html, Output}.
