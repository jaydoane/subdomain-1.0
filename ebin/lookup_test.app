{application, lookup_test,
 [
  {description, "Postfix TCP lookup server test config"},
  {vsn, "0.1"},
  {modules,      [tcp_listener, tcp_lookup_fsm]},
  {registered,   [tcp_server_sup, tcp_listener]},
  {applications, [kernel, stdlib]},
  {mod, {tcp_server_app, []}}, % module name to start the application, plus args
  {env, [{listen_port, 2220}]}
 ]
}.
