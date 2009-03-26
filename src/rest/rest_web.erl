%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for rest.

-module(rest_web).
-author('author <author@example.com>').

-export([start/1, stop/0, respond/1]).

-include("schema.hrl").

%% External API

%% oldstart(Options) ->
%%     {DocRoot, Options1} = get_option(docroot, Options),
%%     Loop = fun (Req) ->
%%                    ?MODULE:loop(Req, DocRoot)
%%            end,
%%     mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

start(Options) ->
    Loop = fun (Req) ->
                   ?MODULE:respond(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

respond(Req) ->
    %%error_logger:info_msg("Received: ~p ~p~n", [Req:get(method), Req:get(path)]),
    respond(Req, Req:get(method), Req:get(path)).

%% Internal API

respond(Req, 'GET', "/domain/" ++ Remainder) ->
    case string:tokens(Remainder, "/") of
        [DomainName, "auth", HexPassDigest] ->
            error_logger:info_msg("Domain: ~p HexPassDigest: ~p~n", [DomainName, HexPassDigest]),
            case db:get_user_for_domain_name(DomainName) of
                [User] ->
                    case db:is_auth_user(User#user.name, mochihex:to_bin(HexPassDigest)) of
                        false ->
                            Req:respond({401, [], "Not authorized"});
                        true ->
                            Req:respond({200, [], "Authorized"})
                    end;
                _ ->
                    Req:respond({400, [], "Domain name does not exist: " ++ DomainName})
            end;
        _ ->
            Req:respond({400, [], "Illegal path structure"})
    end;

respond(Req, 'POST', "/alias") ->
    Body = Req:recv_body(),
    %%error_logger:info_msg("Body: ~p~n", [Body]),
    [HexPassDigest, AliasFrom] = string:tokens(binary_to_list(Body), " "),
    case db:is_alias_available(AliasFrom) of
        false ->
            error_logger:info_msg("Alias already exists: ~p~n", [AliasFrom]),
            Req:respond({200, [], AliasFrom});
        true ->
            case string:tokens(AliasFrom, "@") of
                [Localpart, DomainName] ->
                    case rfc:is_valid_local_part(Localpart) of
                        false -> 
                            Req:respond({400, [], "Invalid localpart: " ++ Localpart});
                        true -> 
                            case db:get_user_for_domain_name(DomainName) of
                                [User] ->
                                    case db:is_auth_user(User#user.name, mochihex:to_bin(HexPassDigest)) of
                                        false ->
                                            Req:respond({401, [], "Not authorized"});
                                        true ->
                                            [Domain] = db:get_domain_by_name(DomainName),
                                            {id, _Id} = db:create_alias(AliasFrom, User#user.email, 
                                                                        Domain#domain.id, 
                                                                        "filler-generated"),
                                            error_logger:info_msg("Created new alias: ~p~n", 
                                                                  [AliasFrom]),
                                            Req:respond({201, [], AliasFrom})
                                    end;
                                _ ->
                                    Req:respond({400, [], "Domain name does not exist: " ++ DomainName})
                            end
                    end;
                _ ->
                    Req:respond({400, [], "Alias must be valid email address: localpart@domain"})
            end
    end;

respond(Req, _Method, Path) ->
    Req:respond({501, [], Path}).

%% loop(Req, DocRoot) ->
%%     "/" ++ Path = Req:get(path),
%%     case Req:get(method) of
%%         Method when Method =:= 'GET'; Method =:= 'HEAD' ->
%%             case Path of
%%                 "cook" ->
%%                     H = mochiweb_cookies:cookie("sid", "SessionId2", []), 
%%                     Req:ok({"text/plain", [H], <<"login success">>}); 
%%                     %%Req:respond({200, [..., H, ...], "..."});
%%                 _ ->
%%                     Req:serve_file(Path, DocRoot)
%%             end;
%%         'POST' ->
%%             case Path of
%%                 _ ->
%%                     Req:not_found()
%%             end;
%%         _ ->
%%             Req:respond({501, [], []})
%%     end.

%% get_option(Option, Options) ->
%%     {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

