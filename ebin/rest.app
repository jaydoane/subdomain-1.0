{application, rest,
 [{description, "rest"},
  {vsn, "0.01"},
  {modules, [
    rest,
    rest_app,
    rest_sup,
    rest_web,
    rest_deps
  ]},
  {registered, []},
  {mod, {rest_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
