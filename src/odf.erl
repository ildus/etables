%% Author: ildus
%% Created: 04.05.2011
%% Description: TODO: Add description to odf
-module(odf).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_content_xml/1, set_content_xml/2, set_content_xml/3]).

%%
%% API Functions
%%

get_content_xml(Filename) ->
    {ok, Files} = zip:extract(Filename, [memory]),
    proplists:get_value("content.xml", Files).

set_content_xml(Content, Filename) ->
    set_content_xml(Content, Filename, Filename).

set_content_xml(Content, Filename, Output) ->
    case Output of
        memory -> set_content_xml(Content, Filename, "result" ++ filename:extension(Filename), [memory]);
        _ when is_list(Output) -> set_content_xml(Content, Filename, Output, [])
    end.

set_content_xml(Content, Filename, Outputname, Options) ->
    {ok, Files} = zip:extract(Filename, [memory]),
    Files1 = [{"content.xml", Content}|proplists:delete("content.xml", Files)],
    zip:create(Outputname, Files1, Options).

%%
%% Local Functions
%%

