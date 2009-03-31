-module(web_index).

-include_lib("nitrogen/include/wf.inc").

-export([main/0, 
         nav/0,
         title/0, 
         body/0,
         event/1]).

main() -> 
    #template {file=filename:join(nitrogen:get_wwwroot(), "template2.html")}.

nav() ->
    #panel {class=nav_panel, 
            body=[
                  #link {text="register", postback=register},
                  " ",
                  #link {text="login", postback=login}
                 ]}.

title() ->
	"Be the master of your subdomain - avoid spam.".

body() ->
    #panel
      {class=mainPanel, body=
       [
        #h3 {text="New! Automatically fill email registration boxes with a customized, valid address"},
        "If you use ", #link {text="firefox", url="http://www.mozilla.com/firefox/"}, ", ",
        "and have installed ", #link {text="greasemonkey", url="https://addons.mozilla.org/en-US/firefox/addon/748"}, ", ",
        "this ", #link {text="email-filler greasemonkey script", url="/email-filler.user.js"},
        " in conjunction with an account on this site, will <em>automatically create a new alias and fill in the email box on pages requiring you enter an email address</em>.",
        #h3 {text="What does this site do?"},
        "Like many people, you may have a single email address you really care about.  But once the spammers get a hold of it, you must rely on various kinds of filters to send spam to a junk mail folder.  And if things get really bad, you may be forced to abandon that address, then painfully inform your correspondents of its replacement.",
        #p {},
        "With this web site, when you ", #link {text="register", postback=register}, " you create a subdomain over which you have complete control.  You can then create as many email aliases as you want and configure them to forward to any email address you like, typically a primary account, on hotmail, gmail, or the like.  In the event that one of your alias addresses falls into the hands of spammers, you can simply disable or delete it.",
        #h3 {text="How can you use it?"},
        "It's recommended to create a new alias for each web site that requires an email address for registration.  For example, if somesite.com wants you to enter an email address, and assuming you've already registered here and created a subdomain yourname.m82.com, you would create a new alias somesite.com@yourname.m82.com which will forward to your default email address.",
        #p {},
        "If you discover later that somesite.com@yourname.m82.com is being used to send you spam, it's trivial to disable it.  You might not want to delete it outright since it's possibly useful information to know which sites have given your email to spammers.",
        #p {},
        "Unfortunately, businesses with shoddy morals aren't the only way your email address might get into the hands of spammers.  Perhaps your friends don't use as much caution with your precious address as you would like by, e.g., mass mailing it with 50 others in the CC.  You can use this site to create individual emails for all your friends, such as joe.smith@yourname.m82.com.",
        #p {},
        "You are only limited by the RFC naming restrictions for ", #link {text="domains", url="http://en.wikipedia.org/wiki/Domain_name#Defined"}, ",  and ", #link {text="email addresses", url="http://en.wikipedia.org/wiki/E-mail_address#RFC_specification"},".",
        #h3 {text="Is this the same as mailinator.com?"},
        "No, but they have the same goal of fighting spam. ", #link {text="Mailinator.com", url="http://mailinator.com"}, " automatically creates temporary email addresses in a single public namespace when an email is sent to it, while anyone can access any mailbox they can think of.  Conversely, with this site you create a permanent subdomain over whose address space you have exclusive control.",
        #p {},
        #h3 {text="How does it work?"},
        "This site runs on ", #link {text="Nitrogen", url="http://nitrogenproject.com"}, ", an ", #link {text="Erlang", url="http://erlang.org/"}, " web application framework.  It uses the ", #link {text="Mnesia", url="http://www.infoq.com/news/2007/08/mnesia"}, " database for persistence.  Mail forwarding is handled by ", #link {text="Postfix", url="http://www.postfix.org/"}, ", which has a ", #link {text="simple TCP lookup protocol", url="http://www.postfix.org/tcp_table.5.html"}, " that queries an additional Erlang server which reads the data entered from this site via distributed Mnesia.",
        #h3 {text="Why?"},
        "I began using this technique with my own domains to control spam a decade ago, and hope that it can help other people reduce their spam.",
        #p {},
        "And Erlang rocks.",
        #p {}
       ]
      }.

event(register) ->
    wf:redirect("/web/register");
event(login) ->
    wf:redirect("/web/login");
event(_) -> ok.
