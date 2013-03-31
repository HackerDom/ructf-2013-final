-module(app_appmod).
-export([out/1]).

-include("/usr/lib/yaws/include/yaws_api.hrl").
-record(user, {id, first_name, last_name, email}).

out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Uri_path = Uri#url.path,
    Path = string:tokens(Uri_path, "/"),
    Method = (Arg#arg.req)#http_request.method,
    try
        {ok, User} = check_auth(Arg),
        {ok, Json} = json2:decode(Arg#arg.clidata),
        out(Path, Method, User, Json)
    of
        Response -> Response
    catch
        _ : {badmatch, {error, badauth}} ->
                return_error(0, "Authentication failure");
        _ : {badmatch, {error, compile_error, Error}} ->
                return_error(2, Error);
        _ : {badmatch, {error, beam_lib, {file_error, _, _}}} ->
                return_error(3, "No such module");
        _ : {badmatch, {error, secfail}} ->
                return_error(4, "Security check fail")
    end.

out(["upload"], 'POST', User, {struct, [{"name", Name}, {"code", ErlangCode}]}) ->
    ModuleName = make_module_name(User, Name),
    {ok, Beam} = code_tools:compile(ModuleName, ErlangCode),
    ok = file:write_file(ModuleName ++ ".beam", Beam),
    return_ok();
out(["exec"], 'POST', User, {struct, [{"name", Name}, {"data", {struct, Data}}]}) ->
    ModuleName = make_module_name(User, Name),
    ok = code_tools:check_security(ModuleName),
    {ok, Result} = mr:exec(ModuleName, Data),
    return_ok(Result);
out(_Path, _Method, _User, _Json) ->
    {status, 404}.

check_auth(Arg) ->
    try
        Session = yaws_api:find_cookie_val("session", Arg),
        Json = json2:encode({struct, [{"session", Session}]}),
        {ok, {_, _, Resp}} =  httpc:request(post, {"http://localhost/user", [], "application/json", Json}, [], []),
        {struct, [{"status", "OK"},
                  {"id", Id}, 
                  {"first_name", FirstName},
                  {"last_name", LastName},
                  {"email", Email}]} = json2:decode(Resp),
        {ok, #user{id = Id, first_name = FirstName, last_name = LastName, email = Email}}
    of
        Ok -> Ok
    catch 
        _:_ -> {error, badauth}
    end.

return_error(Code, Msg) ->
    Json = {struct, [
                {"status", "FAIL"},
                {"error", {struct, [
                    {"code", Code},
                    {"str", Msg}
                ]}}
            ]},
    return_json(json2:encode(Json)).

return_ok(Result) ->
    Json = {struct, [ {"status", "OK"}, {"result", Result} ] },
    return_json(json2:encode(Json)).
return_ok() ->
    Json = {struct, [ {"status", "OK"} ] },
    return_json(json2:encode(Json)).

return_json(Json) ->
    {content,
    "application/json",
    Json}.

make_module_name(User, Name) ->
    hexstring(crypto:md5(User#user.email ++ Name)).

hexstring(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end, 
        binary_to_list(Binary))).
