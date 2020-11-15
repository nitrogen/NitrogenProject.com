% vim: sw=4 ts=4 et
-module (community).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/grid.html" }.

title() -> "Get Involved".

layout() -> 
    #container_12 { body=[
        common:github_fork(),
        #grid_12 { alpha=true, omega=true, class=header, body=common:header(community) },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=headline, body=headline() },
        #grid_clear {},

        #grid_12 { alpha=true, prefix=0, suffix=0, omega=true, body=top() },

        #grid_12 { alpha=true, prefix=0, suffix=0, omega=true, body=articles() },
        #grid_clear {},

        #grid_12 { alpha=true, omega=true, body=common:footer() }
    ]}.

headline() -> 
    "Get Involved".

top_sections() ->
    [
        {true,false,"Discussion","googlegroups.gif","http://groups.google.com/group/nitrogenweb","Subscribe to the <a target=_blank href='http://groups.google.com/group/nitrogenweb'>Nitrogen Google Group</a>."},
        {false,false,"Questions","stackoverflow.gif","http://stackoverflow.com/questions/tagged/nitrogen","Ask and answer questions at <a target=_blank href='http://stackoverflow.com/questions/tagged/nitrogen'>StackOverflow</a>."},
        {false,false,"Roadmap", "trello.png", "http://trello.nitrogenproject.com", "Nitrogen's Current Development and Roadmap at <a target=_blank href='http://trello.nitrogenproject.com'>Trello</a>."},
        {false,false,"Chat","discord.png","https://discord.gg/ppGBxhM","Join our <a target=_new href='https://discord.gg/ppGBxhM'>Discord Server</a>."},
        {false,false,"Bugs","github-issues.png","http://github.com/nitrogen/nitrogen/issues","Submit feature requests and file bugs using <a target=_blank href='http://github.com/nitrogen/nitrogen/issues'>GitHub Issues</a>."},
        {false,true,"Contribute","github.png","http://github.com/nitrogen","Fork Nitrogen on <a target=_blank href='http://github.com/nitrogen/nitrogen'>GitHub</a>. Pull requests are welcome!"}
%        {false,true,"Summaries","ohloh.png","https://www.ohloh.net/p/nitrogen-web-framework","See code, contribution, contributor, trends, and other project summaries on <a target=_blank href='https://www.ohloh.net/p/nitrogen-web-framework'>Ohloh</a>."}
    ].

top() -> 
    [
        #grid_12 { alpha=true, omega=true, body=[
            #hr {}
        ]},
        #grid_clear{},

        lists:map(fun({Alpha,Omega,Header,Logo,LogoUrl,Desc}) ->
            #grid_2 { alpha=Alpha, omega=Omega, body=[
                #h2 { text=Header },
                #p{},
                #link{new=true,url=LogoUrl,body=#image{image="/images/" ++ Logo}},
                #p{},
                Desc
            ]}
        end,top_sections()),

        #grid_clear {},

        #grid_12 { alpha=true, omega=true, body=[
            #hr {}
        ]},

%        #grid_clear {},
        
%        #grid_12 { alpha=true, omega=true, body=[
%            #h2 { text="Donate" },
%            
%            #p{},
%            "
%            Do you use Nitrogen? Help fund Nitrogen development.
%            <p>
%            <a href='http://www.pledgie.com/campaigns/2057'><img alt='Click here to lend your support to: nitrogen and make a donation at www.pledgie.com !' src='http://www.pledgie.com/campaigns/2057.png?skin_name=chrome' border='0' /></a>
%            "
%        ]},

        #grid_clear{},
        
        #grid_12 { alpha=true, omega=true, body=[
            #hr {}
        ]}
    ].

articles() ->
    [

        #h1 { text="Articles and Talks and Presentations" },
  
        #h2 { text="January 2016"},
        #p{},
        #link { text="Raspberry Pi GPIO interface with Nitrogen", url="https://github.com/stuart-thackray/pi_gpio_web/"},
        #p{},
        #h2 { text="December 2015"},
        #p{},
        #link { text="Full-stack Nitrogen hosting platform launches beta", url="https://liquid-nitrogen.org/"},
        #p{},
        #h2{ text="October 2015"},
        #link{ text="Nitrogen with Heroku (slides in French)", url="https://speakerdeck.com/erlangparis/nitrogen-sur-heroku"},
        #p{},
        #h2 { text="April 2015"},
        #p{},
        #link{ text="Video: Roman Galeev: Erlang and Nitrogen Web Framework", url="https://www.youtube.com/watch?v=02NRlE2rmBc"},
        " [",#link{text="Slides", url="http://slides.com/romangaleev/nitrogen#/"},"]",
        #p{},
        #h2 { text="November 2014"},
        #p{},
        #link { text="Mostly Erlang episode about Nitrogen", url="http://mostlyerlang.com/2014/11/05/047-nitrogen-web-framework"},

        #h2 { text="September 2014"},
        #p{},
        #link { text="Video: Building Web-Scale Applications with Nitrogen", url="https://www.youtube.com/watch?v=nlV4gm8SpVA"},
        " [",#link { text="Slides", url="http://slides.sigma-star.com/view/ChicagErlangConference-Nitrogen#/"},"]",
        #p{},
        
        #h2 { text="July 2014"},
        #p{},
        #link { text="Deploying a Nitrogen App on Heroku", url="http://cstar.io/2014/07/02/nitrogen-on-heroku.html"},
        
        #h2{ text="June 2014"},
        #p{},
        #link { text="SimpleBridge Talk at Chicago Erlang User Group", url="http://slides.sigma-star.com/view/SimpleBridge-Full#/"},
        " [", #link { text="demo project", url="https://github.com/nitrogen/simple_bridge_demo"}, "]",

        #h2 { text="February 2014"},
        #p{},
        #link { text="Simple Realtime Synchronization with Nitrogen's #sync_panel Element", url="http://sigma-star.com/blog/post/sync_panel" },

        #h2 { text="October 2013"},
        #p{},
        #link { text="Erlang and Web Scale Panel (Chicago Erlang Factory Lite)", url="http://www.youtube.com/watch?v=hxfyUa9wvZo"},

        #h2 { text="September 2013"},
        #link { text="Introduction to Erlang Web Frameworks (Milwaukee Functional Programming User Group)", url="http://www.youtube.com/watch?v=HdkRzeOjexc"},
        " [", #link { text="Slides", url="http://slides.sigma-star.com/view/Ninja-Erlang-Web#/" },"]",
        #p{},

        #link { text="Why Nitrogen Plugins?", url="http://sigma-star.com/blog/post/why-nitrogen-plugins"},
        #h2 { text="August 2013"},
        #p{},
        #link { text="Embedding Nitrogen into an Existing Application", url="http://sigma-star.com/blog/post/embedding-nitrogen"},
        #p{},
        #h2 { text="June 2013"},
        #p{},
        #link { text="Taming Erlang's New Slim Releases for Nitrogen", url="http://sigma-star.com/blog/post/nitrogen-slim-release"},
        #p{},

        #h2 { text="April 2013"},
        #p{},
        #link { text="N2O, an experimental fork of Nitrogen featuring Websockets", url="https://github.com/5HT/n2o"},
        #p{},

        #h2 { text="March 2013"},
        #p{},
        #link { text="How to build a live datagrid with Nitrogen", url="http://rshestakov.wordpress.com/2013/03/24/how-to-build-a-live-datagrid-with-nitrogen/"},
        #p{},
        #link { text="How to draw graphviz graphs with Nitrogen", url="http://rshestakov.wordpress.com/2013/03/09/how-to-draw-graphviz-graphs-with-nitrogen/"},
        #p{},

        #h2 { text="February 2013"},
        #p{},
        "How Nitrogen processes requests ",
        #link{text="Part 1", url="http://rshestakov.wordpress.com/2013/02/17/how-nitrogen-processes-requests/"}, ", ",
        #link{text="Part 2", url="http://rshestakov.wordpress.com/2013/02/20/how-nitrogen-processes-requests-part-2/"}, ", ",
        #link{text="Part 3", url="http://rshestakov.wordpress.com/2013/02/23/how-nitrogen-processes-requests-part-3/"},
        #p{},

        #h2 { text="January 2013"},
        #p{},
        #link { text="Proper password hashing with Erlang and Nitrogen", url="http://sigma-star.com/blog/post/proper-password-hashing-in-erlang-with"},
        #p{},
        #link { text="How to use HTML5 History API with Nitrogen", url="http://rshestakov.wordpress.com/2013/01/03/how-to-use-html5-history-api-with-nitrogen/"},


        #h2 { text="December 2012"},
        #p{},
        #link { text="How to create custom Nitrogen elements using Tabs control as example.",url="http://rshestakov.wordpress.com/2012/12/31/how-to-create-custom-nitrogen-elements-using-tabs-control-as-example/"},
        #p{},
        #link { text="How to set up a Nitrogen project by hand, by manually specifying the dependencies", url="http://rshestakov.wordpress.com/2012/12/30/how-to-install-nitrogen/"},
        #p{},

        #h2 { text="September 2012"},
        #p{},

        #link { text="IEEE Internet Computing, The Functional Web: The Nitrogen Web Framework", url="http://steve.vinoski.net/pdf/IC-Nitrogen_Web_Framework.pdf"},
        #p{},

        #h2 { text="January 2011" },
        #p{},

        #link { text="Following the Nitrogen Tutorial", url="http://followingthesystemtutorial.blogspot.com/2011/01/nitrogen-erlang-web-application.html"}, 
        #p{},

        #h2 { text="December 2010" },
        #p{},

        #link { text="Nitrogen 2.x presentation at the Chicago Erlang User Group", url="https://docs.google.com/present/view?id=dc37wnrq_7hqpqc6gv"},
        " [", #link{ text="Code", url="https://github.com/choptastic/nitrogen-demo"}, "]",

        #p{},

        #h2 { text="March 2010" },
        
        #p{},
        #link { text="Erlang, Nitrogen and automatic rebuild. (objitsu.com)", url="http://objitsu.com/blog/2010/03/16/erlang-nitrogen-and-automatic-rebuild/" },

        #h2 { text="December 2009" },
        
        #p{},
        #link { text="Erlang Web Development with Nitrogen (sergioveiga.com)", url="http://sergioveiga.com/index.php/2009/12/03/erlang-web-development-with-nitrogen/" },

        #h2 { text="October 2009" },

        #p{},
        "Tristan Sloughter's Nitrogen Presentation at Chicago Erlang User's Group - ",
        #link { text="Part 1", url="http://chicagoerlangusergroup.blogspot.com/2009/10/video-of-tristans-nitrogen-talk-part-1.html" }, ", ",
        #link { text="Part 2", url="http://chicagoerlangusergroup.blogspot.com/2009/10/video-of-tristans-nitrogen-talk-part-2.html" },

        #p{},
        #link { text="redhoterlang.com and the use of Nitrogen", url="http://www.redhoterlang.com/web/plink?id=10807f2903db06ceab888e4b163d675a" },

        #h2 { text="July 2009" },

        #p{},
        #link { text="Build Your Next Web Application with Erlang", url="http://steve.vinoski.net/pdf/IC-Build_Your_Next_Web_Application_with_Erlang.pdf" },
        " - IEEE Internet Computing Magazine, Steve.Vinoski.net",

        #p{},
        #link { text="Using DTL Templates with Nitrogen (fiatdev.com)", url="http://fiatdev.com/2009/07/13/using-dtl-templates-with-nitrogen" },

        #h2 { text="June 2009" },

        #p{},
        #link { text="Advanced Nitrogen Elements (jeremy.marzhissstudios.com)", url="http://jeremy.marzhillstudios.com/index.php/site-news/advanced-nitrogen-elements/" },

        #p{},
        #link { text="Rusty presenting Nitrogen at Erlang Factory in London", url="http://www.erlang-factory.com/conference/London2009/speakers/RustyKlophaus" },
        " - Erlang-Factory.com",

        #p{},
        #link { text="Embedded Webapp on Freerunner (blondon.fr)", url="http://blondon.fr/blog/index.php?post/2009/06/07/freerunner-embedded-webapp" },

        #h2 { text="May 2009" },

        #p{},
        #link { text="Creating Custom Nitrogen Elements (jeremy.marzhissstudios.com)", url="http://jeremy.marzhillstudios.com/index.php/site-news/creating-custom-nitrogen-elements/" },

        #p{},
        #link { text="Erlang Factory 2009 Review (sauria.com)", url="http://www.sauria.com/blog/2009/05/04/erlang-factory-2009/" },

        #h2 { text="April 2009" },

        #p{},
        #link { text="Nitrogen Presentation by Ngoc Dao", url="http://www.slideshare.net/ngocdaothanh/nitrogen-web-framework" },


        #p{},
        #link { text="Rusty presenting Nitrogen at Erlang Factory in San Francisco", url="http://www.erlang-factory.com/conference/SFBayAreaErlangFactory2009/speakers/RustyKlophaus" },
        " - Erlang-Factory.com",

        #p{},
        #link { text="Seethrough and Nitrogen", url="http://www.redhoterlang.com/web/plink?id=bce1408f0f211d3e4951f972b6e9bdbf" },
        " - RedHotErlang.com",  

        #h2 { text="January 2009" },

        #p{},
        #link { text="A Simple Web App using Nitrogen (joeandmotorboat.com)", url="http://www.joeandmotorboat.com/2009/01/08/a-simple-web-app-using-nitrogen/" },

        #h2 { text="December 2008" },
        #p{},
        #link { text="Erlang Web Development Frameworks (medevyoujane.com)", url="http://medevyoujane.com/blog/2008/12/18/erlang-web-development-frameworks.html" },

        #p{},
        #link { text="5 Minute Blog Using Nitrogen and CouchDB (medevyoujane.com)", url="http://medevyoujane.com/blog/2008/12/12/5-minute-blog-using-nitrogen-and-couchdb.html" },

        #p{},
        #link { text="Nitrogen (fiatdev.com)", url="http://fiatdev.com/2008/12/06/nitrogen" }, 

        #h2 { text="November 2008" },
        #p{},
        #link { text="Release of Nitrogen web framework (siteduzero.com)", url="http://translate.google.com/translate?u=http%3A%2F%2Fwww.siteduzero.com%2Fnews-62-30656-sortie-du-framework-web-nitrogen.html&hl=en&ie=UTF-8&sl=fr&tl=en" }, ", ",
        #link { text="French (orig.)", url="http://www.siteduzero.com/news-62-30656-sortie-du-framework-web-nitrogen.html" },

        #h2 { text="October 2008" },
        #link { text="Interview on Nitrogen Web Framework (erlanginside.com)", url="http://erlanginside.com/interview-with-rusty-klophaus-on-the-nitrogen-erlang-web-framework-37" },

        #p{},

        #h1 { text="Projects on Github" },

        #p{},
        #link { text="Raspberry Pi GPIO Web Interface", url="https://github.com/stuart-thackray/pi_gpio_web/"},
        " - See and update the status of the various pins on your Raspberry Pi.",

        #p{},
        #link { text="RISE (by Sovereign Prime)", url="https://github.com/SovereignPrime/RISE"},
        " - An ambitious open source project to create a fully peer-to-peer, distributed, masterless collaboration and project management tool.",
        " [",#link{text="SovereignPrime Homepage", url="http://sovereignprime.com"},"] ",
        #p{},
        #link { text="http://github.com/CoBug/cobug.org", url="http://github.com/CoBug/cobug.org"},
        " - Homepage source code for CoBug, the Colorado BSD User Group",
        " [", #link{text="Homepage", url="http://cobug.org"},
        #p{},
        #link { text="http://github.com/choptastic/sliderl", url="http://github.com/choptastic/sliderl"},
        " - Sliderl a simple markdown slideshow server written with Nitrogen and Reveal.js",

        #p{},
        #link { text="http://github.com/choptastic/LobsterDraw", url="http://github.com/choptastic/LobsterDraw"},
        " - Nitrogen-based drawing game (like Pictionary) made for Spawnfest 2011",
        #p{},
        #link { text="http://github.com/tsloughter/beerenthusiasts", url="http://github.com/tsloughter/beerenthusiasts" },
        " - Homebrewing and beer lovers webapp written in Erlang and using Nitrogen, Couchdb and Mnesia.",

        #p{},
        #link { text="http://github.com/Joony/erlang-nitrogen-user-login", url="http://github.com/Joony/erlang-nitrogen-user-login" },
        " - A simple user login example.",

        #p{},
        #link { text="http://github.com/marksands/Nitro-To-Do/", url="http://github.com/marksands/Nitro-To-Do/" },
        " - Basic To-Do list app in Nitrogen",

        #p{},
        "Wrote an article that you would like to add to this list? <a href='mailto:gumm@sigma-star.com'>Email Jesse</a>."
    ].

