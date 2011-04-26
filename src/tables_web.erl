%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for tables.

-module(tables_web).
-author("Mochi Media <dev@mochimedia.com>").

-include("records.hrl").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    io:format("~nMethod : ~p~n", [Req:get(method)]),
    SessionId = case Req:get_cookie_value("_session_cookie") of
                                        undefined -> session_server:new_session(undefined);
                                        Id -> Id
                                    end,
    Cookie = mochiweb_cookies:cookie("_session_cookie", SessionId),
    Session = session_server:get_session_data(SessionId),
    
    UserId = proplists:get_value(<<"_user_id">>, Session),
    User = case UserId of
        undefined -> undefined;
        Other when is_integer(Other) -> helpers:get_user(UserId)
    end,
    
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    "action" ->                        
                        io:format("~nSession : ~p~n", [Session]),
                        io:format("~nUser : ~p~n", [User]),
                        
                        Data = Req:parse_post(),
                        Json = proplists:get_value("json", Data),
                        Struct = mochijson2:decode(Json),
                        
                        io:format("~nStruct : ~p~n", [Struct]),
                        
                        A = struct:get_value(<<"action">>, Struct),
                        Action = list_to_atom(binary_to_list(A)),
                                
                        Result = if User == undefined andalso Action == authenticate ->
                                        case actions:authenticate(Struct) of
                                            undefined -> {struct, [{<<"error">>, <<"auth_required">>}, 
                                                                   {<<"message">>, <<"Wrong login or password">>}]};
                                            #user{id = UserId1, username = Username1} -> 
                                                session_server:set_session_data(SessionId, <<"_user_id">>, UserId1),
                                                [{<<"success">>, true}]
                                        end;
                                    User == undefined -> {struct, [{<<"error">>, <<"auth_required">>}]};
                                    Action == logout ->
                                        session_server:remove_session_data(SessionId, <<"_user_id">>),
                                        [{<<"success">>, true}];
                                    Action == auth_info ->
                                        {struct, [{<<"username">>, list_to_binary(User#user.username)},
                                                           {<<"user_id">>, User#user.id}]};
                                    is_record(User, user) ->
                                        struct:set_value(<<"user_id">>, User#user.id, Struct),
                                        actions:Action(Struct)
                                 end,

                        DataOut = mochijson2:encode(Result),
                        Req:ok({"application/json", [Cookie], [DataOut]});
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
