%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc tables.

-module(tables).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the tables server.
start() ->
    tables_deps:ensure(),
    ensure_started(crypto),
    application:start(tables).


%% @spec stop() -> ok
%% @doc Stop the tables server.
stop() ->
    application:stop(tables).
