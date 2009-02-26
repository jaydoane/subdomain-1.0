-module(web_domain).
-include_lib("nitrogen/include/wf.inc").

-include("schema.hrl").

-export([main/0, 
         title/0, 
         body/0, 
         event/1]).

-record(rowdata, {row, from, to, note, postback}).

main() -> 
	#template{file="./wwwroot/template.html"}.

title() ->
    {_User, Domain} = wf:user(),
    Domain#domain.name.

get_map() -> 
    #rowdata {row=from_row@id, from=from_cell@text, to=to_cell@text, 
              note=note_cell@text, postback=delete_button@postback}.

split_address(Address) ->
    string:tokens(Address, "@").

%% domain_part(Address) ->
%%     [_Mailbox, Domain] = split_address(Address),
%%     Domain.

mailbox_part(Address) ->
    [Mailbox, _Domain] = split_address(Address),
    Mailbox.

get_data(Domain_id) ->
    Aliases = db:get_aliases_by_domain_id(Domain_id),
    [#rowdata {row = integer_to_list(A#alias.id),
               from = mailbox_part(A#alias.from),
               to = A#alias.to,
               note = A#alias.note,
               postback = {delete, integer_to_list(A#alias.id)}} || A <- Aliases].

body() ->
    {User, Domain} = wf:user(),
    Map = get_map(),
    Data = get_data(Domain#domain.id),
    %%io:format("Data: ~p~n", [Data]),
    Body = [#panel {class=mainPanel, body=
                    [#table {id=alias_table, class=alias, rows=
                             [#tablerow {cells=
                                         [#tableheader {text="From"},
                                          #tableheader {text="To"},
                                          #tableheader {text="Note"},
                                          #tableheader {}
                                         ]},
                              #bind {id=binding, data=Data, map=Map, body=
                                     #tablerow {id=from_row, cells=
                                                [#tablecell {id=from_cell},
                                                 #tablecell {id=to_cell},
                                                 #tablecell {id=note_cell},
                                                 #tablecell {body=#button 
                                                             {id=delete_button, 
                                                              text="delete"}}
                                                ]}}
                             ]},
                     #p{},
                     #span {text="from "},
                     #textbox {id=from_mailbox, next=submit},
                     #span {text="@"},
                     #span {text=Domain#domain.name},
                     #span {text=" to "},
                     #textbox {id=to, text=User#user.email},
                     #span {text="    note "},
                     #textbox {id=note},
                     #br{},
                     #button{id=submit, text="Create", postback=create}
                    ]}
           ],
    wf:wire(submit, from_mailbox, #validate {validators=[#is_required {text="Required"}]}),
    wf:wire(submit, to, #validate {validators=[#is_required {text="Required"}]}),
    wf:render(Body).
	
event(create) ->
    {_User, Domain} = wf:user(),
    [FromMailbox] = wf:q(from_mailbox),
    From = FromMailbox ++ "@" ++ Domain#domain.name,
    [To] = wf:q(to),
    [Note] = wf:q(note),
    {id, Id} = db:create_alias(From, To, Domain#domain.id, Note),
    IdStr = integer_to_list(Id),
    %%io:format("new alias id ~p~n", [Id]),
    wf:insert_bottom(alias_table, 
                     #tablerow {id=IdStr, cells=
                                [#tablecell {text=mailbox_part(From)},
                                 #tablecell {text=To},
                                 #tablecell {text=Note},
                                 #tablecell {body=#button {postback={delete, IdStr}, 
                                                           text="delete"}}
                                ]}
                    ),
    ok;

event({delete, IdStr}) ->
    %%     Message = "Row: " ++ wf:to_list(Id),
    %%     wf:wire(#alert {text=Message}),
    {Id, _} = string:to_integer(IdStr),
    {_User, Domain} = wf:user(),
    case db:get_alias_by_id(Id) of
        [] ->
            io:format("no_alias_with_id ~p~n", [Id]),
            {error, no_alias_with_id};
        [Alias] ->
            case Alias#alias.domain_id =:= Domain#domain.id of
                true ->
                    {atomic, ok} = db:delete_alias_by_id(Id),
                    wf:update(IdStr, #tablerow {id=""}),
                    ok;
                false ->
                    io:format("domain_mismatch ~p ~p~n", [Id, Domain#domain.id]),
                    {error, domain_mismatch}
            end
    end;

event(_) -> ok.
