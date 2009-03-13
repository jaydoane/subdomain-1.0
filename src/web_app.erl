-module (web_app).
-export ([start/2, stop/1, route/1, request/1]).
-behavior(application).

start(_, _) -> nitrogen:start().
%%     {ok, App} = application:get_application(),
%%     {ok, DataNodes} = application:get_env(App, data_nodes),
%%     error_logger:info_msg("joining data nodes ~p~n", [DataNodes]),
%%     mnesia:change_config(extra_db_nodes, DataNodes),

stop(_) -> nitrogen:stop().

%% route/1 lets you define new URL routes to your web pages, 
%% or completely create a new routing scheme.
%% The 'Path' argument specifies the request path. Your
%% function should return either an atom which is the page module
%% to run, or a tuple containing {Module, PathInfo}. PathInfo
%% can be accessed using wf:get_path_info(). 
%%
%% Uncomment the line below to direct requests 
%% from "/web/newroute" to the web_index module:
%%
%% route("/web/newroute") -> web_index;
%%
%% Uncomment the line below to direct requests 
%% from "/web/newroute" to the web_index module, 
%% with trailing PathInfo included:
%%
%% route("/web/newroute/" ++ PathInfo) -> {web_index, PathInfo};

route(Path) -> nitrogen:route(Path).


%% request/1 is executed before every Nitrogen page, and lets
%% you add authentication and authorization. The 'Module' argument
%% is the name of the page module.
%% This function should return either 'ok' if processing can proceed,
%% or it can return a full-fledged page by treating it just like the main function
%% of a page. Alternatively, you can use the wf:redirect* functions to 
%% issue a client-side redirect to a new page.

request(Module) -> 
    %%     io:format("Module: ~p~n", [Module]),
    %%     io:format("Role: ~p~n", [wf:role(auth)]),
    case Module of
        web_index -> ok;
        web_login -> ok;
        web_register -> ok;
        _ ->
            case wf:role(auth) of 
                true ->
                    ok;
                false ->
                    wf:redirect_to_login("/web/login")
            end
    end.
