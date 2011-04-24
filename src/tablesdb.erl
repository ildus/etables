-module(tablesdb).
-compile(export_all).

-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").

reset() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),

    mnesia:create_table(table, [{disc_copies, [node()]}, {attributes, record_info(fields, table)}]),
    mnesia:create_table(user, [{disc_copies, [node()]}, {attributes, record_info(fields, user)}]),
    mnesia:create_table(row, [{disc_copies, [node()]}, {attributes, record_info(fields, row)}]),
    mnesia:create_table(counter, [{disc_copies, [node()]}, {attributes, record_info(fields, counter)}]).

find(Q) ->
    F = fun() ->
            qlc:e(Q)
    end,
    transaction(F).

transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, Result} ->
            Result;
        {aborted, _Reason} ->
            []
    end.

new_id(Key) ->
    mnesia:dirty_update_counter({counter, Key}, 1).

read(Oid) ->
    F = fun() ->
            mnesia:read(Oid)
    end,
    transaction(F).

read_all(Table) ->
    Q = qlc:q([X || X <- mnesia:table(Table)]),
    find(Q). 

write(Rec) ->
    F = fun() ->
            mnesia:write(Rec)
    end,
    mnesia:transaction(F).

delete(Oid) ->
    F = fun() ->
            mnesia:delete(Oid)
    end,
    mnesia:transaction(F).