%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the rest application.

-module(rest_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for rest.
start(_Type, _StartArgs) ->
    rest_deps:ensure(),
    rest_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for rest.
stop(_State) ->
    ok.
