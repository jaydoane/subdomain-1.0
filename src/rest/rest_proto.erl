-module(rest_proto).

-export([
         start/0, 
         stop/0, 
         response/1]).

-include_lib("eunit/include/eunit.hrl").

-include("schema.hrl").

-define(IP, "127.0.0.1").
-define(PORT, "8888").
-define(URL, "http://" ++ ?IP ++ ":" ++ ?PORT).

start() ->
    mochiweb_http:start(
      [{ip, ?IP},
       {port, ?PORT},
       {loop, {?MODULE, response}}]).

stop() ->
    mochiweb_http:stop().

%% start_simple() ->
%%     mochiweb_http:start(
%%       [{ip, ?IP},
%%        {port, ?PORT},
%%        {loop, {?MODULE, simple_response}}]).

%% stop_simple() ->
%%     mochiweb_http:stop().

%% simple_response(Req) ->
%%     simple_response(Req, Req:get(method)).

%% simple_response(Req, Method) ->
%%     Req:ok({"text/plain", atom_to_list(Method)}).

%% http_result(Method) when Method == get; Method == delete ->
%%     parse_result(http:request(Method, {?URL, []}, [], []));
%% http_result(Method) when Method == put; Method == post ->
%%     parse_result(http:request(Method, {?URL, [], [], []}, [], [], default)).

%% parse_result(Result) ->
%%     {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResultBody}} = Result,
%%     ResultBody.

%% ttp(Term) ->
%%     base64:encode_to_string(term_to_binary(Term)).

%% ptt(PlainText) ->
%%     binary_to_term(base64:decode(PlainText)).

%% term_to_plaintext_test_() ->
%%     A = anatom,
%%     B = {a, 23, "abc"},
%%     C = {props, [{a,3},{b,4}]},
%%     [
%%      ?_assert(A == ptt(ttp(A))),
%%      ?_assert(B == ptt(ttp(B))),
%%      ?_assert(C == ptt(ttp(C)))
%%     ].

%% simple_rest_server_test_() ->
%%     {setup,
%%      fun() -> inets:start(), start_simple() end,
%%      fun(_) -> inets:stop(), stop_simple() end,
%%      fun(_) ->
%%              [
%%               ?_assert(http_result(get) =:= "GET"),
%%               ?_assert(http_result(put) =:= "PUT"),
%%               ?_assert(http_result(post) =:= "POST"),
%%               ?_assert(http_result(delete) =:= "DELETE")
%%              ]
%%      end}.

%% atom_to_lower(Atom) ->
%%     list_to_atom(string:to_lower(atom_to_list(Atom))).

%% atom_to_lower_test_() ->
%%     [
%%      ?_assert(get =:= atom_to_lower('GET')),
%%      ?_assert(post =:= atom_to_lower('POST'))
%%     ].

%% result_to_terms(Result) ->
%%     %%?debugFmt("**** result_to_terms ~p~n", [Result]),
%%     {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResultBody}} = Result,
%%     ptt(ResultBody).

%% response(Req, 'GET', "/person/" ++ IdString) ->
%%     Id = list_to_integer(IdString),
%%     Response = crud:retrieve(Id),
%%     Req:ok({"text/plain", ttp(Response)});

%% response(Req, 'DELETE', "/person/" ++ IdString) ->
%%     Id = list_to_integer(IdString),
%%     Response = crud:delete(Id),
%%     Req:ok({"text/plain", ttp(Response)});

response(Req, 'POST', "/alias") ->
    Body = Req:recv_body(),
    %%error_logger:info_msg("Body: ~p~n", [Body]),
    [HexPassDigest, AliasFrom] = string:tokens(binary_to_list(Body), " "),
    case db:is_alias_available(AliasFrom) of
        false ->
            error_logger:info_msg("Alias already exists: ~p~n", [AliasFrom]),
            Req:respond({409, [], "alias already exists"});
        true ->
            case string:tokens(AliasFrom, "@") of
                [_Localpart, DomainName] ->
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
                                    Req:ok({"text/plain", AliasFrom})
                            end;
                        _ ->
                            Req:respond({400, [], "Domain name does not exist: " ++ DomainName})
                    end;
                _ ->
                    Req:respond({400, [], "Alias must be valid email address: localpart@domain"})
            end
    end;

        %%     Person = ptt(Body),
        %%     error_logger:info_msg("Person: ~p~n", [Person]),
        %%     Response = crud:create(Person),
        %%     Req:ok({"text/plain", ttp(Response)});

        %%     [Localpart, Domain] = string:tokens(binary_to_list(Body), "@"),
        %%     error_logger:info_msg("Localpart Domain ~p ~p~n", [From, Domain]),
        %%     Req:ok({"text/plain", Localpart ++ "@" ++ Domain});
        %%     Req:ok({"text/plain", "ok"});

        %% response(Req, 'PUT', "/person/" ++ IdString) ->
        %%     Id = list_to_integer(IdString),
        %%     Body = Req:recv_body(),
        %%     PersonWithNewValues = ptt(Body),
        %%     UpdatedPerson = #person{id = Id, 
        %%                             name = PersonWithNewValues#person.name, 
        %%                             email_address = PersonWithNewValues#person.email_address},
        %%     Response = crud:update(UpdatedPerson),
        %%     Req:ok({"text/plain", ttp(Response)});

response(Req, _Method, Path) ->
    Req:respond({501, [], Path}).

response(Req) ->
    %%error_logger:info_msg("Received: ~p ~p~n", [Req:get(method), Req:get(path)]),
    response(Req, Req:get(method), Req:get(path)).


%% rest_api(Method, Path) when Method == 'GET'; Method == 'DELETE' ->
%%     Result = http:request(atom_to_lower(Method), {?URL ++ Path, []}, [], []),
%%     result_to_terms(Result).

%% rest_api(Method, Path, Data) when Method == 'PUT'; Method == 'POST' ->
%%     Result = http:request(atom_to_lower(Method), {?URL ++ Path, [], [], ttp(Data)}, [], []),
%%     result_to_terms(Result).

rest_api_test_() ->
    {setup,
     fun() -> crud:start(), inets:start(), start() end,
     fun(_) -> stop(), inets:stop(), crud:stop() end,
     fun(_) ->
             [
%%               ?_assert(ok == rest_api('POST', "/person", #person{id = 1})),
%%               ?_assert(already_exists == rest_api('POST', "/person", #person{id = 1})),
%%               ?_assert(#person{id=1} == rest_api('GET', "/person/1")),
%%               ?_assert(ok == rest_api('PUT', "/person/1", #person{name="Ben"})),
%%               ?_assert(#person{id=1, name="Ben"} == rest_api('GET', "/person/1")),
%%               ?_assert(ok == rest_api('DELETE', "/person/1")),
%%               ?_assert(undefined == rest_api('DELETE', "/person/1")),
%%               ?_assert(undefined == rest_api('PUT', "/person/1", #person{id = 2}))
             ]
     end}.

