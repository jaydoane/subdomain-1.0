-module(tcp_db).

-export([
         lookup/1
        ]).

-include("schema.hrl").

lookup(Key) ->
    case string:str(Key, "@") of
        0 -> 
            lookup_domain(Key);
        _ -> 
            lookup_alias(Key)
    end.

lookup_domain(Name) ->
    {atomic, Res} = mnesia:transaction(fun() -> mnesia:read(domain, Name) end),
    [D#domain.name || D <- Res].

lookup_alias(Name) ->
    {atomic, Res} = mnesia:transaction(fun() -> mnesia:read(alias, Name) end),
    [A#alias.to || A <- Res, A#alias.is_active].
