%% http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
-module(tcp_server_app).

-author('saleyn@gmail.com').

-behaviour(application).

%% Internal API
-export([start_client/0]).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(DEFAULT_PORT, 2222).

-include_lib("eunit/include/eunit.hrl").

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(tcp_client_sup, []).

%% Application behaviour callbacks

start(_Type, _Args) ->
    {ok, App} = application:get_application(),
%%     {ok, DataNodes} = application:get_env(App, data_nodes),
%%     error_logger:info_msg("joining data nodes ~p~n", [DataNodes]),
%%     mnesia:change_config(extra_db_nodes, DataNodes),
    ListenPort = get_app_env(listen_port, ?DEFAULT_PORT),
    error_logger:info_msg("~p listening on port ~p~n", [App, ListenPort]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, tcp_lookup_fsm]).

stop(_State) ->
    ok.

%% Supervisor behaviour callbacks

init([Port, Module]) ->
    {ok,
     {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% TCP Listener
       {tcp_server_sup,                          % Id       = internal id
        {tcp_listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [tcp_listener]                           % Modules  = [Module] | dynamic
       },
       %% Client instance supervisor
       {tcp_client_sup,
        {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [Module]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
       }
      ]
     }
    };

init([Module]) ->
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% TCP Client
       {undefined,                               % Id       = internal id
        {Module,start_link,[]},                  % StartFun = {M, F, A}
        temporary,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
       }
      ]
     }
    }.


%% Internal functions

get_app_env(Opt, Default) ->
    {ok, App} = application:get_application(),
    case application:get_env(App, Opt) of
        {ok, Val} -> 
            Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> 
                    Val;
                error ->
                    Default
            end
    end.

test_lookup(Key) ->
    lookup(Key, {127,0,0,1}, 2220).

lookup(Key, Host, Port) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, []),
    Str = "get " ++ Key,
    ok = gen_tcp:send(Sock, Str),
    receive
        {tcp, _Port, Reply} -> 
            ok = gen_tcp:close(Sock),
            Reply
    after
        200 ->
            ok = gen_tcp:close(Sock),
            error
    end.

-define(test_domain, "foo.example.com").

lookup_test_() ->
    {setup,
     fun() ->
             mnesia:start(), 
             application:start(lookup_test), 
             db:create_tables([]) 
     end,
     fun(_) -> 
             application:stop(lookup_test), 
             mnesia:stop()
     end,
     fun(_) ->
             [
              ?_assert("500 not found\n" =:= test_lookup("foo.example.com")),
              ?_assert({id, 1} =:= db:create_domain("foo.example.com", 1)),
              ?_assert("200 foo.example.com\n" =:= test_lookup("foo.example.com")),
              ?_assert("500 not found\n" =:= test_lookup("bar@foo.example.com")),
              ?_assert({id, 1} =:= db:create_alias("bar@foo.example.com", 
                                                   "to@gmail.com", 1, "pass")),
              ?_assert("200 to@gmail.com\n" =:= test_lookup("bar@foo.example.com")),
              ?_assert(false =:= db:toggle_alias_active_by_id(1)),
              ?_assert("500 not found\n" =:= test_lookup("bar@foo.example.com"))
             ]
     end}.
