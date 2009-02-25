-record(counter, {type, count}).
-record(user, {id, name, pass, email, created}). %% last_access
-record(domain, {id, name, user_id, created}).
%%-record(alias, {id, from, to, domain_id, is_active=true, note, created}).
-record(mailmap, {from, to, domain_id, is_active=true, note, created}).

%% -record(user, {name, id, pass, email, created, last_access}).
%% -record(domain, {name, id, user_id, created}).
%% %%-record(alias, {id, from, to, domain_id, is_active=true, note, created}).
%% -record(mailmap, {from, to, domain_id, is_active=true, note, created}).
