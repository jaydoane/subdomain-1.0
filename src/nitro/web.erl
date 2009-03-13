%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(web).
-author('author <author@example.com>').
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the web app and dependencies.
start() ->
    %%rest_deps:ensure(),
    util:ensure_started(crypto),
    util:ensure_started(mnesia),
    application:start(web).

%% @spec stop() -> ok
%% @doc Stop the web app and dependencies.
stop() ->
    Res = application:stop(web),
    application:stop(mnesia),
    application:stop(crypto),
    Res.
