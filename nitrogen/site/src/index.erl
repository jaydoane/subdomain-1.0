%% -*- mode: nitrogen -*-
-module(index).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, 
    nav/0,
    title/0, 
    body/0,
    event/1]).

main() -> #template { file="./site/templates/base.html" }.

nav() ->
    #panel {class=nav_panel, 
        body=[
            #link {text="register", postback=register},
            " ",
            #link {text="login", postback=login}
    ]}.

title() ->
	"Master your subdomain. Evade spam.".

body() ->
    #panel {class=main_panel, body=[
        #h3 {text="What does this do?"},
        "By ", #link {text="registering", postback=register}, " you create a subdomain over which you have complete control.  You can then create as many email aliases as you want, and configure them to forward to any email address you like, typically your primary (gmail, etc.) account.  If one of your aliases falls into the hands of spammers, you can simply disable or delete it.",

        #h3 {text="How can you use it?"},
        "Create a new alias for each web site you use that requires an email address. For instance, if example.com wants you to enter an email address, and you've created the subdomain yourname.m82.com, you could generate a new alias example.com@yourname.m82.com which will forward to your default email address.",
        #p {},
        "If example.com@yourname.m82.com later starts being used to send spam, it's trivial to disable it.  You might not want to delete it outright since it's useful to know which sites have given your email to spammers.",
        #p {},
        "The aliases you choose are only limited by the RFC naming restrictions for ", #link {text="domains", url="http://en.wikipedia.org/wiki/Domain_name#Defined"}, ",  and ", #link {text="email addresses", url="http://en.wikipedia.org/wiki/E-mail_address#RFC_specification"},".",

        #panel {class=greasemonkey_panel, body=[
            #h3 {text="Automate email registration with a customized, valid address"},
            "If you use ", #link {text="firefox", url="http://www.mozilla.com/firefox/"}, ", ",
            "and have installed ", #link {text="greasemonkey", url="https://addons.mozilla.org/en-US/firefox/addon/748"}, ", ",
            "this ", #link {text="email-filler greasemonkey script", url="/email-filler.user.js"},
            " in conjunction with an account on this site, will <em>automatically create a new alias and fill in the email box on pages requiring you enter an email address</em>."
        ]},

        #h3 {text="How does it work?"},
        "This site runs on ", #link {text="Nitrogen", url="http://nitrogenproject.com"}, ", an ", #link {text="Erlang", url="http://erlang.org/"}, " web framework.  It uses the ", #link {text="Mnesia", url="http://www.infoq.com/news/2007/08/mnesia"}, " database for persistence.",
        #p {},
        "Mail forwarding is handled by ", #link {text="Postfix", url="http://www.postfix.org/"}, ", which has a ", #link {text="simple TCP lookup protocol", url="http://www.postfix.org/tcp_table.5.html"}, " that queries distributed Mnesia via an Erlang server.",
        #p {}
    ]
    }.

event(register) ->
    wf:redirect("/register");

event(login) ->
    wf:redirect("/login");

event(_) -> ok.
