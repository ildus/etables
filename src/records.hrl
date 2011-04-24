-record(user, {
    id, %% user id, int
    username, %% string
    password, %% password, string
    is_admin = false %% admin indicator
}).
-record(table, {    
    id, %% table id, int
    name, %% table name, string
    columns = [{int, <<"id">>}], %% table columns [{str, "col1"}, {datetime, "col2"}, {number, "col3"}]
    owner, %% owner(creator) id of this table
    users = [] %% users can access to the table
}).
-record(row, {
    id, %% row id, int
    table_id, %% int
    data %% row data, list, [{"col1", "value"}, {"col2", "value"}, {"col3", "value"},]
}).
-record(counter, {key, value}).