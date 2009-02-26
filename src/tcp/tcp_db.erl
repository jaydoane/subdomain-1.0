-module(tcp_db).

-export([lookup/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("schema.hrl").

lookup(Key) ->
    case string:str(Key, "@") of
        0 -> lookup_domain(Key);
        _ -> lookup_alias(Key)
    end.

lookup_domain(Name) ->
    F = fun() -> qlc:e(qlc:q([X#domain.name || X <- mnesia:table(domain), 
                                               X#domain.name =:= Name])) end,
    mnesia:transaction(F).

lookup_alias(From) ->
    F = fun() -> qlc:e(qlc:q([X#alias.to || X <- mnesia:table(alias), 
                                              X#alias.from =:= From,
                                              X#alias.is_active =:= true])) end,
    mnesia:transaction(F).

%% create_tables() ->
%%     mnesia:create_table(map, [{attributes, record_info(fields, map)}, {disc_copies, [node()]}]).
%% %%mnesia:create_table(map, [{attributes, record_info(fields, map)}]).

%% delete_tables() ->
%%     mnesia:delete_table(map).
    
%% insert_map(From, To) ->
%%     Row = #mailmap{from=From, to=To},
%%     F = fun() -> mnesia:write(Row) end,
%%     mnesia:transaction(F).

%% get_value(From) ->
%%     F = fun() -> qlc:e(qlc:q([X#mailmap.to || X <- mnesia:table(mailmap), X#mailmap.from =:= From])) end,
%%     mnesia:transaction(F).

%% delete_map(From) ->
%%     Oid = {mailmap, From},
%%     F = fun() -> mnesia:delete(Oid) end,
%%     mnesia:transaction(F).
