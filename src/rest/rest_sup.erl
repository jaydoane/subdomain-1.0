%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the rest application.

-module(rest_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,
    Port = case init:get_argument(port) of 
               error -> 
                   case application:get_env(listen_port) of
                       undefined -> 7000;
                       Other -> Other
                   end;
               {ok, [[PortStr]]} -> 
                   list_to_integer(PortStr) 
           end,
    error_logger:info_msg("configured on port: ~p, ip: ~p~n", [Port, Ip]),
    WebConfig = [{ip, Ip},
                 {port, Port}],
    %%{docroot, rest_deps:local_path(["priv", "www"])}],
    Web = {rest_web,
           {rest_web, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.
