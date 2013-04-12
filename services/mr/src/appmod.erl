-module(mr_appmod).
-export([upload/3, exec/3, parse_arg/1, check_auth/1]).

-include("../deps/yaws/include/yaws_api.hrl").
-record(user, {id, first_name, last_name, email}).

upload(User, Name, ErlangCode) ->
    ModuleName = make_module_name(User, Name),
    {ok, Beam} = code_tools:compile(ModuleName, ErlangCode),
    file:write_file(ModuleName ++ ".beam", Beam).
exec(User, Name, Data) ->
    ModuleName = list_to_atom(make_module_name(User, Name)),
    ok = code_tools:check_security(ModuleName),
    {module, ModuleName} = code:load_file(ModuleName),
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
        {ok, "200", _, JsonResp} =  ibrowse:send_req("http://192.168.1.111/user",
                                                     [{"X-Requested-With", "XMLHttpRequest"},
                                                      {"Content-Type", "application/json"}], post, Json),
        {ok, {struct, Resp}} = json2:decode_string(JsonResp),
        "OK" = proplists:get_value("status", Resp),
        {struct,[{"$oid",Uid}]} = proplists:get_value("uid", Resp), %% workaround
        FirstName = proplists:get_value("first_name", Resp),
        LastName = proplists:get_value("last_name", Resp),
        Email = proplists:get_value("email", Resp),
        {ok, #user{id = Uid, first_name = FirstName, last_name = LastName, email = Email}}
    of
        Ok -> Ok
    catch 
        _:_ -> {error, badauth}
    end.

make_module_name(User, Name) ->
    hexstring(crypto:md5(User#user.id ++ Name)).

hexstring(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end, 
        binary_to_list(Binary))).
