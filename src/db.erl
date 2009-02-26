%% {ok,_} = mnesia:change_config(extra_db_nodes,[dbm@alu]).
%% connects current node to dbm@alu

-module(db).

-export([
         init/0,
         create_user/3,
         create_domain/2,
         create_alias/4,
         get_table/1,
         is_username_available/1,
         get_user_by_name/1,
         is_domain_available/1,
         get_domains_by_user_id/1,
         get_aliases_by_domain_id/1,
         is_auth_user/2
        ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("schema.hrl").

-define(sha_len, 160).
-define(counter_incr, 1).

init() ->
    init([node()]).

init(DiskNodes) ->
    mnesia:create_schema(DiskNodes),
    mnesia:start(),
    create_tables(DiskNodes),
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
                
%% write(Row) ->
%%     mnesia:transaction(fun() -> mnesia:write(Row) end).

transact_create(T) ->
    case mnesia:transaction(T) of
        {atomic, exists} ->
            {error, exists};
        {atomic, Id} ->
            {id, Id}
    end.
    
get_table(Name) when is_atom(Name) ->
    do(qlc:q([X || X <- mnesia:table(Name)])).

get_user_by_name(Name) ->
    do(qlc:q([X || X <- mnesia:table(user), X#user.name =:= Name])).

is_username_available(Name) ->    
    case length(get_user_by_name(Name)) of
        1 ->
            false;
        0 ->
            true
    end.

create_user(Name, Password, Email) ->
    LowerName = string:to_lower(Name),
    T = fun() ->
                case mnesia:read(user, LowerName) of
                    [] ->
                        Id = mnesia:dirty_update_counter(counter, user, ?counter_incr),
                        <<Digest:?sha_len>> = crypto:sha(Password),
                        Row = #user{id=Id, name=LowerName, email=Email, pass=Digest, 
                                    created=erlang:localtime(), 
                                    last_access=erlang:localtime()},
                        mnesia:write(Row),
                        Id;
                    [_] ->
                        exists
                end
        end,
    transact_create(T).


is_auth_user(Name, Password) ->
    <<Digest:?sha_len>> = crypto:sha(Password),
    case length(do(qlc:q([X#user.name || X <- mnesia:table(user), 
                                         X#user.name =:= Name, 
                                         X#user.pass =:= Digest]))) of
        1 ->
            true;
        0 ->
            false
    end.

is_domain_available(Name) ->
    LowerName = string:to_lower(Name),
    case length(do(qlc:q([X#domain.name || X <- mnesia:table(domain), 
                                           X#domain.name =:= LowerName]))) of
        1 ->
            false;
        0 ->
            true
    end.

create_domain(Name, User_id) ->
    T = fun() ->
                case mnesia:read(domain, Name) of
                    [] ->
                        Id = mnesia:dirty_update_counter(counter, domain, ?counter_incr),
                        Row = #domain{id=Id, name=string:to_lower(Name), user_id=User_id, 
                                      created=erlang:localtime()},
                        mnesia:write(Row),
                        Id;
                    [_] ->
                        exists
                end
        end,
    transact_create(T).

get_domains_by_user_id(User_id) ->
    do(qlc:q([X || X <- mnesia:table(domain), X#domain.user_id =:= User_id])).

create_alias(From, To, Domain_id, Note) ->
    T = fun() ->
                case mnesia:read(alias, From) of
                    [] ->
                        Id = mnesia:dirty_update_counter(counter, alias, ?counter_incr),
                        Row = #alias{from=From, id=Id, to=To, domain_id=Domain_id, 
                                     note=Note, created=erlang:localtime()},
                        mnesia:write(Row),
                        Id;
                    [_] ->
                        exists
                end
        end,
    transact_create(T).

get_aliases_by_domain_id(Domain_id) ->
    do(qlc:q([X || X <- mnesia:table(alias), X#alias.domain_id =:= Domain_id])).

crud_test_() ->   
  {setup,
   fun() -> crypto:start(), mnesia:start(), create_tables([]) end,
   fun(_) -> mnesia:stop(), crypto:stop() end,
   fun(_) ->
           [
            ?_assert(true =:= is_username_available("jay")),
            ?_assert({id, 1} =:= create_user("jay", "pass", "jay@foo.com" )),
            ?_assert(false =:= is_username_available("jay")),
            ?_assert({error, exists} =:= create_user("jay", "pass", "jay@foo.com" )),
            ?_assert(true =:= is_domain_available("jay.example.com")),
            ?_assert([] =:= get_domains_by_user_id(1)),
            ?_assert({id, 1} =:= create_domain("jay.example.com", 1)),
            ?_assert(false =:= is_domain_available("jay.example.com")),
            ?_assert({error, exists} =:= create_domain("jay.example.com", 1)),
            ?_assert(1 =:= length(get_domains_by_user_id(1))),
            ?_assert({id, 1} =:= create_alias("spammer.com@jay.example.com", "jay@foo.com", 
                                              1, "mylogin:mypass")),
            ?_assert({error, exists} =:= create_alias("spammer.com@jay.example.com", 
                                                      "jay@foo.com", 1, "mylogin:mypass"))
           ]
   end}.
