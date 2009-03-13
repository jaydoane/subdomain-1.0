%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(util).
-author('author <author@example.com>').

-export([ensure_started/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
