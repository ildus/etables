%% Author: ildus
%% Created: 05.07.2011
%% Description: TODO: Add description to migrations
-module(migrations).

%%
%% Include files
%%

-include("records.hrl").

%%
%% Exported Functions
%%
-export([m0001_add_sortfield/0]).

%%
%% API Functions
%%

m0001_add_sortfield() ->
    TransformFunction = fun (Old) ->
                                 {table, Id, Name, Columns, Owner, Users, Template} = Old,
                                 #table{columns = Columns, id = Id, name = Name, owner = Owner, template = Template, users = Users, sortfield = id}
                        end,
    {atomic, ok} = mnesia:transform_table(table, TransformFunction, record_info(fields, table)).

%%
%% Local Functions
%%

