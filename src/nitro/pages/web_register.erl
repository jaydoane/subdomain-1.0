-module(web_register).

-include_lib("nitrogen/include/wf.inc").

-include("schema.hrl").

-export([main/0, 
         title/0, 
         body/0,
         event/1]).

main() -> 
    #template {file=filename:join(nitrogen:get_wwwroot(), "template2.html")}.

title() ->
	"register".

body() ->
    Body = 
        [#panel 
         {class=mainPanel, body=
          [#panel 
           {body=
            [#label {text="username/subdomain"},
             #textbox {id=username, next=email},
             #span {text="."},
             #dropdown {id=base_domain, options=
                        [#option {text=D#domain.name, value=D#domain.name} || 
                            D <- db:get_base_domains()]}]},
           #p{},
           #label {text="default email"},
           #textbox {id=email, next=password},
           #p{},
           #label {text="password"},
           #password {id=password, next=confirm},
           #p{},
           #label {text="confirm"},
           #password {id=confirm, next=register},
           #p{},
           #button {id=submit, text="register", postback=register}
          ]}
        ],
    wf:wire(submit, username, 
            #validate {validators=
                       [#is_required { text="Required."},
                        #custom{text="Unavailable", tag=ignored_tag,
                                function=fun is_username_available/2},
                        #custom{text="Invalid", tag=ignored_tag,
                                function=fun is_name_valid/2}
                       ]}),
    wf:wire(submit, password, 
            #validate {validators=[#is_required {text="Required."}]}),
    wf:wire(submit, email, 
            #validate {validators=[#is_required {text="Required."}]}),
	wf:wire(submit, confirm, 
            #validate {validators=
                       [#is_required {text="Required."},
                        #confirm_password {password=password, 
                                           text="Passwords must match."}]}),
    wf:render(Body).
	
register(Username, Password, Email, BaseDomain) ->
    {id, User_id} = db:create_user(Username, Password, Email),
    DomainName = Username ++ "." ++ BaseDomain,
    {id, Domain_id} = db:create_domain(DomainName, User_id),
    {id, _Alias_id} = db:create_alias("example.com@" ++ DomainName, Email, Domain_id, 
                                     "sample alias - click here to edit"),
    {User_id, Domain_id}.

event(register) ->
    case register(hd(wf:q(username)), hd(wf:q(password)), 
                  hd(wf:q(email)), hd(wf:q(base_domain))) of
        {_User_id, _Domain_id} ->
            wf:redirect("login");
        _ ->
            wf:flash("Error") % fixme
    end,
    ok;

event(_) -> ok.

is_username_available(_Tag, Name) ->
    Domain = Name ++ "," ++ hd(wf:q(base_domain)),
    db:is_username_available(Name) and db:is_domain_available(Domain).

is_name_valid(_Tag, Name) ->
    rfc:is_valid_domain_label(Name).
