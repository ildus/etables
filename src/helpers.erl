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

-export([new_user/3, get_user/1]).

%%
%% API Functions
%%

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

%%
%% Local Functions
%%

