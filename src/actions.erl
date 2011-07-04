%% Author: ildus
%% Created: 20.04.2011
%% Description: TODO: Add description to actions
-module(actions).

%%
%% Include files
%%

-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").

%%
%% Exported Functions
%%
-export([new_table/1, tables_list/1, delete_table/1, add_row/1, table_rows/1, edit_table/1, edit_row/1,
         delete_row/1]).
-export([authenticate/1]).

%%
%% API Functions
%%

new_table(S) ->
    change_table(S, tablesdb:new_id(tables), true).

edit_table(S) ->
    change_table(S, struct:get_value(<<"table_id">>, S), false).

%% --------------------------------------------------------------------
%% Function: change_table/3
%% Description: Change table structure
%% --------------------------------------------------------------------
change_table(S, Id, IsNew) when is_integer(Id) ->
    User = struct:get_value(<<"user">>, S),
    Name = struct:get_value(<<"table_name">>, S),
    Columns = case struct:get_value(<<"columns">>, S) of
                  undefined -> [];
                  List when is_list(List) -> parse_columns(List)
              end,
    
    CanChange = 
        if
            IsNew -> true;
            true -> helpers:has_permission({change, User, Id})
        end,
    Res = 
        if 
            IsNew ->
                NewTable = #table{id = Id, name = Name, columns = Columns, owner = User#user.id},
                tablesdb:write(NewTable);
            CanChange ->
                OriginalTable = helpers:get_table(Id),
                ChangedTable = OriginalTable#table{name = Name, columns = Columns},
                tablesdb:write(ChangedTable);
            true -> throw(access_denied)
        end,
    io:format("Res: ~p~n", [Res]),
    [{<<"id">>, Id}].
    
tables_list(S) ->
    User = struct:get_value(<<"user">>, S),
    Tables = 
        case User#user.is_admin of
            true -> tablesdb:read_all(table);
            false ->
                Q = qlc:q([X || X <- mnesia:table(table), X#table.owner == User#user.id]),
                tablesdb:find(Q)
        end,
             
    lists:map(fun(F) -> {struct, [
                                  {<<"id">>, F#table.id}, 
                                  {<<"name">>, F#table.name},
                                  {<<"columns">>, reparse_columns(F#table.columns)},
                                  {<<"template">>, list_to_binary(F#table.template)}]}
              end, Tables).

table_rows(S) ->
    TableId = struct:get_value(<<"table_id">>, S),
    Start = struct:get_value(<<"start">>, S),
    Limit = struct:get_value(<<"limit">>, S),
    Q = qlc:q([X || X <- mnesia:table(row), X#row.table_id == TableId]),
    [TableInfo] = tablesdb:read({table, TableId}),
    SortField = TableInfo#table.sortfield,
    Q1 = if 
            SortField == id -> qlc:sort(Q, {order, fun (Row1, Row2) -> Row1#row.id > Row2#row.id end});
            is_integer(SortField) -> qlc:sort(Q, {order, 
                                   fun (Row1, Row2) -> 
                                            proplists:get_value(Row1#row.data, SortField) > 
                                                proplists:get_value(Row2#row.data, SortField) end})
    end,
       
    Rows = tablesdb:find(Q1),
    Rows1 = lists:sublist(Rows, Start + 1, Limit),
    Records = lists:map(fun(R) -> {struct, [
                                    {<<"id">>, R#row.id},
                                    {<<"table_id">>, R#row.table_id},                                  
                                    {<<"data">>, R#row.data}]}
              end, Rows1),
    {struct, [{<<"records">>, Records}, {<<"total_count">>, length(Rows)}]}.

delete_table(S) ->
    Id = struct:get_value(<<"table_id">>, S),
    {atomic, ok} = tablesdb:delete({table, Id}),
    [{<<"deleted">>, Id}].

add_row(S) ->
    change_row(S, tablesdb:new_id(rows)).

edit_row(S) ->
    Id = struct:get_value(<<"row_id">>, S),
    change_row(S, Id).

delete_row(S) ->
    Id = struct:get_value(<<"row_id">>, S),
    {atomic, ok} = tablesdb:delete({row, Id}),
    [{<<"deleted">>, Id}].

change_row(S, Id) when is_integer(Id) ->
    TableId = struct:get_value(<<"table_id">>, S),
    {struct, RowData} = S,
    RowData1 = repair_column_ids(RowData),
    {atomic, ok} = tablesdb:write(#row{id = Id, table_id = TableId, data = RowData1}),
    [{<<"changed">>, Id}].

authenticate(S) ->
    Username = binary_to_list(struct:get_value(<<"username">>, S)),
    Password = binary_to_list(struct:get_value(<<"password">>, S)),
    Q = qlc:q([X || X <- mnesia:table(user), X#user.username == Username]),
    case tablesdb:find(Q) of
        [User|_] ->
            if User#user.password == Password -> User;
               true -> undefined
            end;
        [] -> undefined
    end.

%%
%% Local Functions
%%

parse_columns(Columns) ->
    parse_columns(Columns, []).

parse_columns([], Res) ->
    Res;
parse_columns([Col|Other], Res) ->
    [Name, Type, IsFilter, Id, Atom] = Col,
    io:format("~p", [Id]),
    ColumnId = if Id == 0 -> tablesdb:new_id(columns);
                  is_integer(Id) -> Id;
                  is_list(Id) -> list_to_integer(Id);
                  is_binary(Id) -> list_to_integer(binary_to_list(Id))
               end,    
    ColType = list_to_atom(binary_to_list(Type)),
    ColAtom = list_to_atom(binary_to_list(Atom)),
    parse_columns(Other, [{ColType, Name, IsFilter, ColumnId, ColAtom}|Res]).

reparse_columns(Columns) ->
    reparse_columns(Columns, {struct, []}).

reparse_columns([], S) ->
    S;
reparse_columns([Col|Other], S) ->
    {ColTypeA, Name, IsFilter, ColumnId, ColAtomA} = Col,
    ColId = list_to_binary(integer_to_list(ColumnId)),
    ColType = list_to_binary(atom_to_list(ColTypeA)),
    ColAtom = list_to_binary(atom_to_list(ColAtomA)),
    {struct, Res} = S,
    reparse_columns(Other, {struct, [{ColId, {struct, [{<<"id">>, ColId},
                                                        {<<"name">>, Name}, 
                                                        {<<"type">>, ColType},
                                                        {<<"is_filter">>, IsFilter},
                                                        {<<"atom">>, ColAtom}]
                                              }
                                     }|Res]
                           }).

repair_column_ids([{BinaryId, Value}|Row], Res) ->
    try list_to_integer(binary_to_list(BinaryId)) of
        Id -> repair_column_ids(Row, [{Id, Value}|Res])
    catch
        _:_ -> repair_column_ids(Row, Res)
    end;
    
repair_column_ids([], Res) ->
    Res.

repair_column_ids(Val) when is_list(Val) ->
    repair_column_ids(Val, []).