{application, web, 
 [
  {description, "subdomain manager webapp"},
  {mod, {web_app, []}},
  {env, 
   [
    {platform, mochiweb}, %% {inets|yaws|mochiweb}
    {port, 8000},
    {session_timeout, 200},
    {sign_key, "Rand0mSIGN_KEY"},
    {wwwroot, "./wwwroot"}
   ]}
 ]}.
