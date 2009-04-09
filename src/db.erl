-module(db).

-import(string, [to_lower/1]).

-export([
         init/0,
         create_tables/1,
         get_table/1,
         write/1,
         create_user/3,
         login_user/1,
         get_user_by_name/1,
         is_username_available/1,
         is_auth_user/2,
         change_default_email/2,
         change_password/2,
         create_domain/2,
         create_base_domains/1,
         is_domain_available/1,
         get_domains_by_user_id/1,
         get_domain_by_name/1,
         get_user_for_domain_name/1,
         get_base_domains/0,
         create_alias/4,
         is_alias_available/1,
         get_aliases_by_domain_id/1,
         get_alias_by_id/1,
         toggle_alias_active_by_id/1,
         change_aliases_to/3,
         change_alias_note/2,
%%          user_passwords_int_to_bin/0,
         delete_alias_by_id/1
        ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("schema.hrl").

-define(counter_incr, 1).
-define(base_domains, ["m82.com"]).

init() ->
    init([node()], ?base_domains).

init(DiskNodes, DomainNames) ->
    mnesia:create_schema(DiskNodes),
    mnesia:start(),
    create_tables(DiskNodes),
    create_base_domains(DomainNames),
    mnesia:stop().

create_tables(DiskNodes) ->
    mnesia:create_table(counter, [{attributes, record_info (fields, counter)}, 
                                  {disc_copies, DiskNodes}]),
    mnesia:create_table(user, [{attributes, record_info (fields, user)},
                               {index, [id]}, {disc_copies, DiskNodes}]),
    mnesia:create_table(domain, [{attributes, record_info (fields, domain)},
                                 {index, [id, user_id]}, {disc_copies, DiskNodes}]),
    mnesia:create_table(alias, [{attributes, record_info (fields, alias)},
                                {index, [id, domain_id]}, {disc_copies, DiskNodes}]).

do(Q) ->
    {atomic, Val} = mnesia:transaction(fun() -> qlc:e(Q) end),
    Val.
                
write(Row) ->
    mnesia:transaction(fun() -> mnesia:write(Row) end).

create_entry(Type, Key, RowGen) ->
    T = fun() ->
                case mnesia:read(Type, Key) of
                    [] ->
                        Id = mnesia:dirty_update_counter(counter, Type, ?counter_incr),
                        mnesia:write(RowGen(Id)),
                        Id;
                    [_] ->
                        exists
                end
        end,
    case mnesia:transaction(T) of
        {atomic, exists} ->
            {error, exists};
        {atomic, Id} ->
            {id, Id}
    end.
    
get_table(Name) when is_atom(Name) ->
    do(qlc:q([X || X <- mnesia:table(Name)])).

get_user_by_name(Name) ->
    LowerName = to_lower(Name),
    do(qlc:q([X || X <- mnesia:table(user), X#user.name =:= LowerName])).

is_username_available(Name) -> 
    case length(get_user_by_name(Name)) of
        1 ->
            false;
        0 ->
            true
    end.

create_user(Name, Password, Email) ->
    Key = to_lower(Name),
    RowGen = fun(Id) ->
                     Digest = crypto:sha(Password),
                     #user{id=Id, name=Key, email=Email, pass=Digest, 
                           created=erlang:localtime(), 
                           last_access=erlang:localtime()}
             end,
    create_entry(user, Key, RowGen).

change_default_email(Email, User_id) ->
    case do(qlc:q([X || X <- mnesia:table(user), 
                        X#user.id =:= User_id,
                        X#user.email /= Email])) of
        [User] ->
            Update = User#user{email=Email},
            db:write(Update),
            ok;
        [] ->
            unchanged
    end.
        
change_password(Password, User_id) ->
    case do(qlc:q([X || X <- mnesia:table(user), 
                        X#user.id =:= User_id])) of
        [User] ->
            Digest = crypto:sha(Password),
            Update = User#user{pass=Digest},
            db:write(Update),
            ok;
        [] ->
            unchanged
    end.
        
login_user(User) ->
    Update = User#user{last_access=erlang:localtime()},
    db:write(Update),
    Update.

is_auth_user(Name, Password) when is_list(Password) ->
    is_auth_user(Name, crypto:sha(Password));

is_auth_user(Name, Digest) when is_binary(Digest) ->
    LowerName = to_lower(Name),
    case length(do(qlc:q([X#user.name || X <- mnesia:table(user), 
                                         X#user.name =:= LowerName, 
                                         X#user.pass =:= Digest]))) of
        1 ->
            true;
        0 ->
            false
    end.

get_user_for_domain_name(Name) ->
    LowerName = to_lower(Name),
    do(qlc:q([U || D <- mnesia:table(domain), 
                   D#domain.name =:= LowerName,
                   U <- mnesia:table(user),
                   U#user.id =:= D#domain.user_id])).    

%% one-time data repair
%% user_passwords_int_to_bin() ->
%%     UpdatedUsers = do(qlc:q([U#user {
%%                                pass=mochihex:to_bin(mochihex:to_hex(U#user.pass))}
%%                              || U <- mnesia:table(user)])),
%%     mnesia:transaction(
%%       fun() -> 
%%               [mnesia:write(U) || U <- UpdatedUsers]
%%       end),
%%     UpdatedUsers.

is_domain_available(Name) ->
    LowerName = to_lower(Name),
    case length(do(qlc:q([X#domain.name || X <- mnesia:table(domain), 
                                           X#domain.name =:= LowerName]))) of
        1 ->
            false;
        0 ->
            true
    end.

create_domain(Name, User_id) ->
    Key = to_lower(Name),
    RowGen = fun(Id) ->
                     #domain{name=Key, id=Id, user_id=User_id, created=erlang:localtime()}
             end,
    create_entry(domain, Key, RowGen).

create_base_domains([H|T]) ->
    create_domain(H, 0),
    create_base_domains(T);
create_base_domains([]) ->
    ok.

get_base_domains() ->
    get_domains_by_user_id(0).

get_domain_by_name(Name) ->
    F = fun() ->
                mnesia:read(domain, Name)
        end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

get_domains_by_user_id(User_id) ->
    do(qlc:q([X || X <- mnesia:table(domain), X#domain.user_id =:= User_id])).

create_alias(From, To, Domain_id, Note) ->
    Row = fun(Id) ->
                  #alias{from=From, id=Id, to=To, domain_id=Domain_id, 
                         note=Note, created=erlang:localtime()}
          end,
    create_entry(alias, From, Row).

is_alias_available(From) ->
    T = fun() ->
                case mnesia:read(alias, From) of
                    [_Alias] ->
                        false;
                    [] ->
                        true
                end
        end,
    {atomic, Val} = mnesia:transaction(T),
    Val.

get_alias_by_id(Id) ->
    do(qlc:q([X || X <- mnesia:table(alias), X#alias.id =:= Id])).

delete_alias_by_id(Id) ->
    case get_alias_by_id(Id) of
        [] ->
            {error, no_alias_with_id};
        [Alias] ->
            Oid = {alias, Alias#alias.from},
            mnesia:transaction(fun() -> mnesia:delete(Oid) end)
    end.

get_aliases_by_domain_id(Domain_id) ->
    do(qlc:q([X || X <- mnesia:table(alias), X#alias.domain_id =:= Domain_id])).

toggle_alias_active_by_id(Id) ->
    [Alias] = db:get_alias_by_id(Id),
    State = not Alias#alias.is_active,
    Update = Alias#alias {is_active=State},
    db:write(Update),
    State.

change_alias_note(Id, Note) ->
    [Alias] = db:get_alias_by_id(Id),
    Update = Alias#alias {note=Note},
    db:write(Update),
    ok.

change_aliases_to(To, To, _Domain_id) ->
    [];
change_aliases_to(OldTo, NewTo, Domain_id) ->
    UpdatedAliases = do(qlc:q([X#alias {to=NewTo} || 
                                  X <- mnesia:table(alias),
                                  X#alias.domain_id =:= Domain_id,
                                  X#alias.to =:= OldTo])),
    mnesia:transaction(
        fun() -> 
                [mnesia:write(A) || A <- UpdatedAliases]
        end),
    [A#alias.id || A <- UpdatedAliases].

crud_test_() ->   
  {setup,
   fun() -> crypto:start(), mnesia:start(), create_tables([]) end,
   fun(_) -> mnesia:stop(), crypto:stop() end,
   fun(_) ->
           [
            ?_assert(true =:= is_username_available("jay")),
            ?_assert({id, 1} =:= create_user("jay", "pass", "jay@foo.com" )),
            ?_assert(false =:= is_username_available("Jay")),
            ?_assert({error, exists} =:= create_user("jay", "pass", "jay@foo.com" )),
            ?_assert(true =:= is_domain_available("jay.example.com")),
            ?_assert([] =:= get_domains_by_user_id(1)),
            ?_assert({id, 1} =:= create_domain("Jay.example.com", 1)),
            ?_assert(false =:= is_domain_available("jay.example.com")),
            ?_assert({error, exists} =:= create_domain("jay.example.com", 1)),
            ?_assert(1 =:= length(get_domains_by_user_id(1))),
            ?_assert(1 =:= length(get_domain_by_name("jay.example.com"))),
            ?_assert(1 =:= length(get_user_for_domain_name("jay.example.com"))),
            ?_assert({id, 1} =:= create_alias("spammer.com@jay.example.com", 
                                              "jay@foo.com", 1, "mylogin:mypass")),
            ?_assert({id, 2} =:= create_alias("SPAMMER.COM@jay.example.com", 
                                              "jay@foo.com", 1, "mylogin:mypass")),
            ?_assert({error, exists} =:= create_alias("SPAMMER.COM@jay.example.com", 
                                                      "jay@foo.com", 1, "mylogin:mypass")),
            ?_assert([] =:= change_aliases_to("bogus", "new@unused.com", 1)),
            ?_assert([] =:= change_aliases_to("bogus", "new@unused.com", 0)),
            ?_assert(2 == length(change_aliases_to("jay@foo.com", "jay@new.com", 1))),
            ?_assert(unchanged =:= change_default_email("jay@foo.com", 1)),
            ?_assert(ok =:= change_default_email("jay@new.com", 1))
           ]
   end}.
