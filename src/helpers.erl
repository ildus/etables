%% Author: ildus
%% Created: 23.04.2011
%% Description: TODO: Add description to helpers
-module(helpers).

%%
%% Include files
%%

-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").

%%
%% Exported Functions
%%

-export([new_user/3, get_user/1, has_permission/1]).
-export([process_template/2, get_template_data/3, get_table/1, set_template/2]).
-export([get_row/1]).

%%
%% API Functions
%%

%% Users API

new_user(Username, Password, IsAdmin) when is_list(Username), is_list(Password), is_boolean(IsAdmin) ->
    Id = tablesdb:new_id(users),
    Q = qlc:q([X || X <- mnesia:table(user), X#user.username == Username]),
    case tablesdb:find(Q) of
        [] -> tablesdb:write(#user{id = Id, username = Username, password = Password, is_admin = IsAdmin});
        [_] -> {error, already_exists}
    end.

get_user(UserId) ->
    case tablesdb:read({user, UserId}) of
        [User] -> User;
        [] -> undefined
    end.

get_table(TableId) ->
    case tablesdb:read({table, TableId}) of
        [Table] -> Table;
        [] -> throw(not_found)
    end.

get_row(RowId) ->
    case tablesdb:read({row, RowId}) of
        [Row] -> Row;
        [] -> throw(not_found)
    end.

set_template(TableId, TemplateName) ->
    Table = get_table(TableId),
    NewTable = Table#table{template = TemplateName},
    tablesdb:write(NewTable).

has_permission({_, User, _}) when User#user.is_admin ->
    true;
has_permission({change, User, TableId}) ->
    Table = get_table(TableId),
    Table#table.owner == User#user.id;
has_permission({read, User, TableId}) ->
    Table = get_table(TableId),
    io:format("Permission table: ~p~n", [Table#table.users]),
    if 
        Table#table.owner == User#user.id -> true;
        true -> lists:member(User#user.id, Table#table.users)
    end.

%% Templates API

%% @spec process_template(FileName, Variables) -> {ok, {Name, Body}}
%% @doc Odf templater, Variables through erlydtl, Filename is full path to .odt file
process_template(Filename, Variables) ->
    Content = odf:get_content_xml(Filename),
    {ok, odf_template} = erlydtl:compile(Content, odf_template),
    {ok, IOList} = odf_template:render(Variables),
    Rendered = iolist_to_binary(IOList),
    odf:set_content_xml(Rendered, Filename, memory).

get_template_data(User, TableId, RowId) when is_integer(TableId), is_integer(RowId) ->
    case has_permission({read, User, TableId}) of
        true -> 
            Table = get_table(TableId),
            Row = get_row(RowId),
            {Table#table.template, prepare_row_data(Table#table.columns, Row#row.data)};
        false ->
            throw(access_denied)
    end.

%%
%% Local Functions
%%

prepare_row_data([], _) ->
    [];
prepare_row_data([Column| Columns], Data) ->
    {_, _, _, Id, Atom} = Column,
    [{Atom, proplists:get_value(Id, Data)} | prepare_row_data(Columns, Data)].