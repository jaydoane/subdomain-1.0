-module(web_login).
-include_lib("nitrogen/include/wf.inc").

-include("schema.hrl").

-compile(export_all).

main() -> 
	#template{file="./wwwroot/template.html"}.

title() ->
	"login".

body() ->
    Body = 
        [
         #panel{class=mainPanel, body=
                [#label {text="username"},
                 #textbox {id=username, postback=login, next=password},
                 #p{},
                 #label { text="password" },
                 #password {id=password, postback=login, next=submit},
                 #p{},
                 #button {id=submit, text="Login", postback=login}]},
         #link {text="register", postback=register}
        ],
    wf:wire(submit, username, #validate {validators=[#is_required {text="Required."}]}),
    wf:wire(submit, password, #validate {validators=[#is_required {text="Required."}]}),
    wf:render(Body).
	
authenticate(Username, Password) ->
    case db:is_auth_user(Username, Password) of
        true -> 
            [User] = db:get_user_by_name(Username),
            {auth, User};
        false -> invalid
    end.

event(register) ->
    wf:redirect("register");

event(login) ->
    case authenticate(hd(wf:q(username)), hd(wf:q(password))) of
        {auth, User} ->
            db:login_user(User),
            [Domain] = db:get_domains_by_user_id(User#user.id),
            wf:user({User, Domain}),
            wf:role(auth, true),
            wf:redirect_from_login("domain");
        _ ->
            wf:flash("Incorrect")
    end;

event(_) -> ok.
