%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(rest).
-author('author <author@example.com>').
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the rest server.
start() ->
    rest_deps:ensure(),
    util:ensure_started(crypto),
    util:ensure_started(mnesia),
    application:start(rest).

%% @spec stop() -> ok
%% @doc Stop the rest server.
stop() ->
    Res = application:stop(rest),
    application:stop(mnesia),
    application:stop(crypto),
    Res.
