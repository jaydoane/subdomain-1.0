-module(web_index).
-include_lib("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template {file="./wwwroot/template.html"}.

title() ->
	"Be the master of your subdomain - sidestep spam.".

body() ->
    #panel 
      {class=mainPanel, body=
       [
        #h3 {text="What does this site do?"},
        "If you are like most people, you may have a single email address that you really care about.  Once the spammers get ahold of it, you rely on various kinds of filters to send the spam to a junk mail folder.  And if things get really bad, you may be forced to abandon that address, and painfully inform your correspondents of its replacement.",
        #p {},
        "With this web site, when you register you create a subdomain over which have complete control.  You can then create as many email aliases as you want and configure them to forward to any email address you like, typically your primary account.  In the event that one of your alias addresses falls into the hands of spammers, you can simply disable or delete it.",
        #h3 {text="How can you use it?"},
        "It's recommended to create a new alias for each web site that requires an email address for registration.  For example, if somesite.com wants you to enter an email address, and assuming you've already registered here and created a subdomain yourname.m82.com, you would create a new alias somesite.com@yourname.m82.com which will forward to your default email address.",
        #p {},
        "If you discover later that somesite.com@yourname.m82.com is being used to send you spam, it's trivial to disable it.  You might not want to delete it outright since it's possibly useful information to know which sites have given your email to spammers.",
        #p {},
        "Unfortunately, businesses with shoddy morals aren't the only way your email address might get into the hands of spammers.  Perhaps your friends don't use as much caution with your precious address as you would like by, e.g., sending it with 50 others in the CC: of a mass mailing.  You can use this site to create individual emails for all your friends, such as joe.smith@yourname.m82.com.",
        #p {},
        "You are only limited by the RFC naming restrictions for ", #link {text="domains", url="http://en.wikipedia.org/wiki/Domain_name#Defined"}, ",  and ", #link {text="email addresses", url="http://en.wikipedia.org/wiki/E-mail_address#RFC_specification"},".",
        #h3 {text="How does it work?"},
        "This website runs on ", #link {text="Nitrogen", url="http://nitrogenproject.com"}, ", an ", #link {text="Erlang", url="http://erlang.org/"}, " web framework.  It uses the ", #link {text="Mnesia", url="http://www.infoq.com/news/2007/08/mnesia"}, " database for persistence.  Mail forwarding is handled by ", #link {text="Postfix", url="http://www.postfix.org/"}, ", which has a simple TCP lookup protocol that queries an additional Erlang server which reads the data entered from this site via distributed Mnesia.",
        #h3 {text="Why?"},
        "I began hosting my own domains a decade ago, and have used this technique to monitor and control spam ever since.  Unlimited control of email addresses was a bonus for going through the trouble to learn how to administer a mail server. My hope was that a simple web interface would make it accessible to more people who would find it useful. And Erlang rocks.",
        #p {},
        #link {text="register", postback=register},
        " ",
        #link {text="login", postback=login}
       ]
      }.

event(register) ->
    wf:redirect("/web/register");
event(login) ->
    wf:redirect("/web/login");
event(_) -> ok.
