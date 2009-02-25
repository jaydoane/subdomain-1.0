-module(web_register).
-include_lib("nitrogen/include/wf.inc").
-compile(export_all).

-define(BASE_DOMAIN, "m82.com").

main() -> 
	#template{file="./wwwroot/template.html"}.

title() ->
	"register".

body() ->
    Body = [
            #panel{ class=mainPanel, 
                    body=[
                          #label { text="username" },
                          #textbox { id=username, next=email },
                          #p{},
                          #label { text="email" },
                          #textbox { id=email, next=password },
                          #p{},
                          #label { text="password" },
                          #password { id=password, next=confirm },
                          #p{},
                          #label { text="confirm" },
                          #password { id=confirm, next=register },
                          #p{},
                          #button {id=submit, text="Register", postback=register}
                         ]}
           ],
    wf:wire(submit, username, #validate { 
                      validators=[#is_required { text="Required." },
                                  #custom{text="Unavailable", tag=some_tag,
                                          function=fun is_username_available/2}]}),
    wf:wire(submit, password, #validate { validators=[#is_required { text="Required." }]}),
    wf:wire(submit, email, #validate { validators=[#is_required { text="Required." }]}),
	wf:wire(submit, confirm, #validate {
                      validators=[#is_required { text="Required." },
                                  #confirm_password { password=password, text="Passwords must match." }
	]}),
    
    wf:render(Body).
	
register(Username, Password, Email) ->
    Id = db:create_user(Username, Password, Email),
    db:create_domain(Username ++ "." ++ ?BASE_DOMAIN, Id),
    {valid, Id}.

event(register) ->
    io:format("register ~n"),
    case register(hd(wf:q(username)), hd(wf:q(password)), hd(wf:q(email))) of
        {valid, _Id} ->
            wf:user(hd(wf:q(username))),
            wf:redirect("login");
        _ ->
            wf:flash("Error") %fixme
    end,
    ok;

event(_) -> ok.

is_username_available(_Tag, Value) ->
    db:is_username_available(Value).
