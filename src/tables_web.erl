%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for tables.

-module(tables_web).
-author("Mochi Media <dev@mochimedia.com>").

-include("records.hrl").
-include_lib("kernel/include/file.hrl").

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
                    "parse" when User /= undefined ->
                        Data = Req:parse_qs(),
                        TableId = proplists:get_value("table", Data),
                        RowId = proplists:get_value("row", Data),
                        
                        {RelPath, Variables} = helpers:get_template_data(User, 
                                                                              list_to_integer(TableId),                                                                              
                                                                              list_to_integer(RowId)),
                        TemplatePath = filename:join(tables_deps:local_path(["priv", "templates"]), RelPath),
                        {ok, {Name, Body}} = helpers:process_template(TemplatePath, Variables),
                        Req:ok({"application/odf",
                                            [{"Content-Disposition", "attachment; filename=" ++ Name}],
                                            Body});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    "handle_template" when User /= undefined ->
                        ValidExtensions = [".odt"],
                        Data = Req:parse_qs(),
                        TableId = list_to_integer(proplists:get_value("table_id", Data)),
                        upload_template(Req, tables_deps:local_path(["priv", "templates"]), ValidExtensions, TableId);
                    "action" ->
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
                                            #user{id = UserId1} -> 
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
                                        ActionStruct = struct:set_value(<<"user">>, User, Struct),
                                        ActionResult = actions:Action(ActionStruct),
                                        ActionResult
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
            
            if 
                What == not_found -> Req:not_found();
                What == access_denied -> Req:respond({403, [{"Content-Type", "text/plain"}], "forbidden, sorry\n"});
                true -> Req:respond({500, [{"Content-Type", "text/plain"}],
                            "request failed, sorry\n"})
            end
    end.

%% Internal API

upload_template(Req, DestinationDir, ValidExtensions, TableId) ->
    
    % Setup the file handler and parse the multipart data
    FileHandler = fun(Filename, ContentType) -> handle_file(Filename, ContentType) end,
    Files = mochiweb_multipart:parse_form(Req, FileHandler),
    
    {OriginalFilename, _, TempFilename} = proplists:get_value("file_template", Files),
    
    % Check the file extension is valid
    case lists:member(filename:extension(OriginalFilename), ValidExtensions) of
        true ->
            TemplateName = "template_" ++ integer_to_list(erlang:phash2(make_ref())) ++ ".odt",
            Destination = filename:join(DestinationDir, TemplateName),
            io:format("Destination: ~p~n", [Destination]),
            case file:copy(TempFilename, Destination) of                
                {ok, _} ->
                    file:delete(TempFilename),
                    catch helpers:set_template(TableId, TemplateName),
                    Req:ok({"text/html", [], ["<textarea>ok</textarea>"]});                    
                {error, Reason} ->
                    % Something went wrong
                    file:delete(TempFilename),
                    Req:ok({"text/html", [], "<textarea>"++atom_to_list(Reason)++"</textarea>"})
            end;
            
        false ->
            % User tried to upload a file with an invalid extension
            file:delete(TempFilename),
            Req:ok({"text/html", [], "<textarea>invalid extension</textarea>"})
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%% @doc Handle a file. A 'chunk' handling function will be returned.
handle_file(Filename, ContentType) ->
    TempFilename = "/tmp/" ++ atom_to_list(?MODULE) ++ integer_to_list(erlang:phash2(make_ref())),
    {ok, File} = file:open(TempFilename, [raw, write]),
    chunk_handler(Filename, ContentType, TempFilename, File).

%% @doc Return a function for handling chunks of data. If the 'eof' atom is
%% passed to the returned function then the file will be closed and details
%% returned. Otherwise, a function will be returned which will be able to
%% handle the next chunk of data.
chunk_handler(Filename, ContentType, TempFilename, File) ->
    fun(Next) ->
        case Next of
            
            eof ->
                % End of part: close file and return details of the upload
                file:close(File),
                {Filename, ContentType, TempFilename};
                
            Data ->
                % More data to write to the file
                file:write(File, Data),
                chunk_handler(Filename, ContentType, TempFilename, File)
        end
    end.

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
