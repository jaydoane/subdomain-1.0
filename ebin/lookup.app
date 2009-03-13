{application, lookup,
 [
  {description, "Postfix TCP lookup server"},
  {vsn, "0.1"},
  {modules,      [tcp_listener, tcp_lookup_fsm]},
  {registered,   [tcp_server_sup, tcp_listener]},
  {applications, [kernel, stdlib]},
  {mod, {tcp_server_app, []}} % module to start, plus args
 ]
}.
