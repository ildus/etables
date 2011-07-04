-record(user, {
    id, %% user id, int
    username, %% string
    password, %% password, string
    is_admin = false %% admin indicator
}).
-record(table, {    
    id, %% table id, int
    name, %% table name, string
    columns = [], %% table columns [{str, "col1"}, {datetime, "col2"}, {number, "col3"}]
    owner, %% owner(creator) id of this table
    users = [], %% users that can access to the table
    template = [],
    sortfield = id
}).
-record(row, {
    id, %% row id, int
    table_id, %% int
    data %% row data, proplist, [{id, "value"}, {director, "value"}, {worker, "value"},]
}).
-record(counter, {key, value}).