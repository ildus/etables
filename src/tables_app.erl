%% @author Mochi Media <dev@mochimedia.com>
%% @copyright tables Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the tables application.

-module(tables_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for tables.
start(_Type, _StartArgs) ->
    tables_deps:ensure(),
    tables_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for tables.
stop(_State) ->
    ok.
