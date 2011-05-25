%% -*- mode: nitrogen -*-
-module(login).

-include_lib("nitrogen_core/include/wf.hrl").

-include("schema.hrl").

-export([main/0, 
         nav/0,
         title/0, 
         body/0,
         event/1]).

main() -> #template { file="./site/templates/base.html" }.

nav() ->
    #panel {class=nav_panel, 
            body=[
                  #link {text="register", postback=register}
                 ]}.

title() ->
	"login".

body() ->
    wf:wire(submit, username, #validate {validators=[#is_required {text="Required."}]}),
    wf:wire(submit, password, #validate {validators=[#is_required {text="Required."}]}),
    [
        #panel {class=main_panel, body=[
            #flash {},
            #label {text="username"},
            #textbox {id=username, postback=login, next=password},
            #p{},
            #label { text="password" },
            #password {id=password, postback=login, next=submit},
            #p{},
            #button {id=submit, text="Login", postback=login}
        ]}
    ].
	
authenticate(Username, Password) ->
    case db:is_auth_user(Username, Password) of
        true -> 
            [User] = db:get_user_by_name(Username),
            {auth, User};
        false -> invalid
    end.

event(register) ->
    wf:redirect("/register");

event(login) ->
    case authenticate(wf:q(username), wf:q(password)) of
        {auth, User} ->
            db:login_user(User),
            [Domain] = db:get_domains_by_user_id(User#user.id),
            wf:user({User, Domain}),
            wf:role(auth, true),
            wf:redirect_from_login("domain");
        _ ->
            wf:flash("Incorrect username or password")
    end;

event(_) -> ok.
