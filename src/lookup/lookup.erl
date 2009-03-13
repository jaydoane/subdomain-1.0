%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(lookup).
-author('author <author@example.com>').
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the rest server.
start() ->
    util:ensure_started(mnesia),
    application:start(lookup).

%% @spec stop() -> ok
%% @doc Stop the rest server.
stop() ->
    Res = application:stop(lookup),
    application:stop(mnesia),
    Res.
