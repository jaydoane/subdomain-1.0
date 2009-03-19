-module(web_domain).

-include_lib("nitrogen/include/wf.inc").

-include("schema.hrl").

-import(string, [to_lower/1]).

-export([main/0, 
         title/0, 
         body/0,
         inplace_textbox_event/2,
         event/1]).

-record(rowdata, {id, from, to, note_text, note_tag, active_checked, active_postback, delete}).

main() -> 
    #template {file=filename:join(nitrogen:get_wwwroot(), "template2.html")}.

title() ->
    {_User, Domain} = wf:user(),
    Domain#domain.name.

get_map() -> 
    #rowdata {id=alias_id@id,
              from=from_cell@text,
              to=to_cell@text,
              note_text=note_edit@text,
              note_tag=note_edit@tag,
              active_checked=active_checkbox@checked,
              active_postback=active_checkbox@postback,
              delete=delete_button@postback}.

get_data(Domain_id) ->
    Aliases = lists:sort(db:get_aliases_by_domain_id(Domain_id)),
    [#rowdata {id = integer_to_list(A#alias.id),
               from = local_part(A#alias.from),
               to = A#alias.to,
               note_text = case A#alias.note of "" -> " "; Note -> Note end,
               note_tag = integer_to_list(A#alias.id),
               active_checked = A#alias.is_active,
               active_postback = {toggle_active, integer_to_list(A#alias.id)},
               delete = {delete, integer_to_list(A#alias.id)}} || A <- Aliases].

split_address(Address) ->
    string:tokens(Address, "@").

local_part(Address) ->
    [Localpart, _Domain] = split_address(Address),
    Localpart.

body() ->
    {User, Domain} = wf:user(),
    Map = get_map(),
    Data = get_data(Domain#domain.id),
    %%io:format("Data: ~p~n", [Data]),
    Body = [#panel {class=mainPanel, body=
                    [#table {id=alias_table, class=alias, rows=
                             [#tablerow {cells=
                                         [#tableheader {text="from"},
                                          #tableheader {text="to"},
                                          #tableheader {text="note"},
                                          #tableheader {text="active"},
                                          #tableheader {}
                                         ]},
                              #bind {id=binding, data=Data, map=Map, 
                                     transform=fun alternate_color/2,
                                     body=
                                     #tablerow {id=alias_id, cells=
                                                [#tablecell {id=from_cell},
                                                 #tablecell {id=to_cell},
                                                 #tablecell {body=#inplace_textbox {id=note_edit, tag=note_tag}},
                                                 #tablecell {body=#checkbox {id=active_checkbox}},
                                                 #tablecell {body=#button {id=delete_button, 
                                                                           text="delete"}}
                                                ]}}]},
                     #p {},
                     #p {},
                     #panel {id=create_panel, class=createPanel, body=
                             [#span {text="from: "},
                              #textbox {id=from_localpart, next=submit},
                              #span {text="@"},
                              #span {text=Domain#domain.name},
                              #span {text="  to: "},
                              #textbox {id=to, text=User#user.email},
                              #span {text="  note: "},
                              #textbox {id=note},
                              #br{},
                              #br{},
                              #button{id=submit, text="create alias", postback=create}]},
                     #p {},
                     #panel {id=change_panel, class=createPanel, show_if=false, body=
                             [#span {text="change all 'to' addresses from: "},
                              #textbox {id=old_to, text=User#user.email},
                              #span {text="  to: "},
                              #textbox {id=new_to, text=""},
                              #p {},
                              #checkbox {id=default_email_checkbox, checked=true},
                              #span {text=" also change default email"},
                              #br{},
                              #br{},
                              #button{id=change_to, text="change", postback=change_to}]}
                    ]},
            #link {text="logout", postback=logout}
           ],
    wf:wire(submit, from_localpart, #validate {validators=
                                               [#is_required {text="Required"},
                                                #custom{text="Exists", tag=ignored_tag,
                                                        function=fun is_alias_available/2},
                                                #custom{text="Invalid", tag=ignored_tag,
                                                        function=fun is_name_valid/2}
                                               ]}),
    wf:wire(submit, to, #validate {validators=[#is_required {text="Required"}]}),
%% this breaks everything if uncommented and change_panel show_if=false
%%     wf:wire(change_to, old_to, #validate {validators=[#is_required {text="Required"}]}),
%%     wf:wire(change_to, new_to, #validate {validators=[#is_required {text="Required"}]}),
    wf:render(Body).

alternate_color(DataRow, Acc) when Acc == []; Acc==odd ->
    {DataRow, even, {alias_id@style, "background-color: #eee;"}};
alternate_color(DataRow, Acc) when Acc == even ->
    {DataRow, odd, {alias_id@style, "background-color: #ccc;"}}.

%% inplace_textbox brokenly replaces spaces with ;nbsp
remove_nbsp_code(Str) ->
    string:join(string:tokens(Str, "\302\240"), " ").

inplace_textbox_event(IdStr, RawValue) ->
    {Id, _} = string:to_integer(IdStr),
    Value = remove_nbsp_code(RawValue),
    %%io:format("inplace_textbox_event/2: ~p ~p~n", [Id, Value]),
    ok = db:change_alias_note(Id, Value),
    Value.

event(logout) ->
    wf:user(undefined),
    wf:role(auth, false),
    wf:redirect("/web/login");

event(change_to) ->
    {User, Domain} = wf:user(),
    [OldTo] = wf:q(old_to),
    [NewTo] = wf:q(new_to),
    DidDefaultChange = 
        case wf:q(default_email_checkbox) of
            ["on"] -> 
                case db:change_default_email(NewTo, User#user.id) of
                    ok ->
                        %%io:format("default changed~n", []),
                        wf:user({User#user{email=NewTo}, Domain}),
                        true;
                    _ ->
                        false
                end;
            [] -> 
                false
        end,
    %%io:format("change_to: ~p ~p ~p~n", [OldTo, NewTo, ChangeDefault]),
    ChangedIds = db:change_aliases_to(OldTo, NewTo, Domain#domain.id),
    %%io:format("changed_ids: ~p~n", [ChangedIds]),
    DidIdsChange = 
        case ChangedIds of
            [] ->
                false;
            _ ->
                true
        end,
    case DidDefaultChange or DidIdsChange of
        true ->
            wf:redirect("domain");
        false ->
            ok
    end,
    ok;

event(create) ->
    {_User, Domain} = wf:user(),
    [Localpart] = wf:q(from_localpart),
    From = Localpart ++ "@" ++ Domain#domain.name,
    [To] = wf:q(to),
    [Note] = wf:q(note),
    {id, Id} = db:create_alias(From, To, Domain#domain.id, Note),
    IdStr = integer_to_list(Id),
    wf:insert_bottom(
      alias_table, 
      #tablerow {
        id=IdStr, cells=
        [#tablecell {text=local_part(From)},
         #tablecell {text=To},
         #tablecell {body=#inplace_textbox {id=note_edit, text=Note, tag=IdStr}},
         #tablecell {body=#checkbox {
                       checked=true, postback={toggle_active, IdStr}}},
         #tablecell {body=#button {
                       text="delete", postback={delete, IdStr}}}
        ]}),
    wf:set(from_localpart, ""),
    wf:set(note, ""),
    ok;

event({delete, IdStr}) ->
    {Id, _} = string:to_integer(IdStr),
    case db:get_alias_by_id(Id) of
        [_Alias] ->
            {atomic, ok} = db:delete_alias_by_id(Id),
            wf:update(IdStr, #tablerow {id=""}),
            ok;
        [] ->
            wf:wire(#alert {text=wf:f("no alias with id: ~w", [Id])}),
            {error, no_alias_with_id}
    end;

event({toggle_active, IdStr}) ->
    {Id, _} = string:to_integer(IdStr),
    db:toggle_alias_active_by_id(Id),
    ok;

event(Other) -> 
    wf:wire(#alert {text=wf:f("unhandled event: ~w", [Other]) }),
    ok.

is_alias_available(_Tag, Localpart) ->
    {_User, Domain} = wf:user(),
    From = Localpart ++ "@" ++ Domain#domain.name,
    db:is_alias_available(From).

is_name_valid(_Tag, Localpart) ->
    rfc:is_valid_local_part(Localpart).
