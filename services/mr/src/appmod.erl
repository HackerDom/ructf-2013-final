-module(appmod).
-export([upload/3, exec/3, parse_arg/1, check_auth/1]).

-include("../deps/yaws/include/yaws_api.hrl").
-record(user, {id, first_name, last_name, email}).

upload(User, Name, ErlangCode) ->
    ModuleName = make_module_name(User, Name),
    {ok, Beam} = code_tools:compile(ModuleName, ErlangCode),
    file:write_file("priv/code/" ++ ModuleName ++ ".beam", Beam).

exec(User, Name, Data) ->
    ModuleName = list_to_atom(make_module_name(User, Name)),
    ok = code_tools:check_security(ModuleName),
    case code:is_loaded(ModuleName) of 
        {file, _} -> ok;
        false     -> 
            {module, ModuleName} = code:load_file(ModuleName)
    end,
    mr:exec(ModuleName, Data).

parse_arg(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Uri_path = Uri#url.path,
    Path = string:tokens(Uri_path, "/"),
    Method = (Arg#arg.req)#http_request.method,
    {Path, Method}.

check_auth(Arg) ->
    try
        Session = yaws_api:find_cookie_val("session", Arg),
        Json = json2:encode({struct, [{"session", Session}]}),
        {ok, "200", _, JsonResp} =  ibrowse:send_req("http://localhost/user",
                                                     [{"X-Requested-With", "XMLHttpRequest"},
                                                      {"Content-Type", "application/json"}], post, Json),
        {ok, {struct, Resp}} = json2:decode_string(JsonResp),
        ["OK", Uid, FirstName, LastName, Email] = [proplists:get_value(Val, Resp)
                || Val <- ["status", "uid", "first_name", "last_name", "email"]],
        {ok, #user{id = Uid, first_name = FirstName, last_name = LastName, email = Email}}
    of
        Ok -> Ok
    catch 
        _:_ -> {error, badauth}
    end.

make_module_name(User, Name) ->
    "code" ++ hexstring(crypto:md5(User#user.id ++ Name)).

hexstring(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end, 
        binary_to_list(Binary))).
