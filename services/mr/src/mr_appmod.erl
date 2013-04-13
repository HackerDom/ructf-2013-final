-module(mr_appmod).
-export([out/1]).

-include("../deps/yaws/include/yaws_api.hrl").

out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Uri_path = Uri#url.path,
    Path = string:tokens(Uri_path, "/"),
    Method = (Arg#arg.req)#http_request.method,
    ContentType = yaws_api:get_header(Arg#arg.headers, 'Content-Type'),
    Appmod = case ContentType of
            "application/json" -> json_appmod;
            _                  -> web_appmod
    end,
    try
        {ok, User} = appmod:check_auth(Arg),
        case ContentType of
            "application/json" -> Appmod:out(Path, Method, User, Arg);
            _                  -> Appmod:out(Path, Method, User, Arg)
        end
    of
        Response -> Appmod:return_ok(Response)
    catch
        _ : {badmatch, {error, badauth}} ->
                Appmod:return_error(0, "Authentication failure");
        _ : {badmatch, {error, compile_error, Error}} ->
                Appmod:return_error(1, Error);
        _ : {badmatch, {error, beam_lib, {file_error, _, _}}} ->
                Appmod:return_error(2, "No such module");
        _ : secfail ->
                Appmod:return_error(3, "Security check fail");
        _ : nopage ->
                Appmod:return_error(4, "No such page: 404");
        _ : _ ->
                Appmod:return_error(5, "Something go wrong :(")
                %Appmod:return_error(5, E)
    end.
