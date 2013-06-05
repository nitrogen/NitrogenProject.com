% vim: sw=4 ts=4 et
-module (learn).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/grid.html" }.

title() -> "Learn More".

headline() -> "Learn More".

layout() ->
    #container_12 { body=[
        common:github_fork(),
        #grid_12 { class=header, body=common:header(learn) },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=headline, body=headline() },
        #grid_clear {},

        #grid_5 { alpha=true, prefix=1, class=pad_right, body=left() },
        #grid_5 { omega=true, suffix=1, body=right() },
        #grid_clear {},

        #grid_12 { body=common:footer() }
    ]}.
        

left() -> 
    [
        #p{},
        "
        Nitrogen brings interactive web applications to Erlang.
        ",
        #p{},
        
        #h2 { text="Event-Driven Development" },
        #p{},
        "
        Nitrogen uses an event-driven model built on top of Erlang pattern matching. Nitrogen
        allows you to tag elements with any Erlang term, and then act on the tag in server-side
        code when the user clicks on, hovers over, or otherwise interacts with the element. 
        Catching the event is as simple as writing an Erlang function.
        ",
        
        #h2 { text="Brainlessly Easy Ajax" },
        #p{},
        "
        Nitrogen allows you to update or replace a section of your page using Ajax in just one 
        line of code. Most importantly, Nitrogen lets you use the same consistent syntax to both
        build AND update the page.
        ",
        
        #h2 { text="Ridiculously Simple Comet" },
        #p{},
        "
        Nitrogen includes Comet support, allowing you to build interactive web applications
        that push data to the browser. By simply wrapping your function call
        in one line of code, you can turn a synchronous function into a long-running asynchronous function.
        ",
        
        #h2 { text="Complex Interfaces: Drag/Drop/Sort" },
        #p{},
        "
        With Nitrogen, you can let a user interact with your application in complex ways.
        Nitrogen lets you tag the draggable, droppable, or sortable elements, and then respond to 
        the resulting events in server-side code as easily as you would respond to a click. 
        ",
        
        #h2 { text="Flexible Templating" },
        #p{},
        "
        Nitrogen includes a simple but powerful template system, allowing you to define
        a consistent style for your application. You can add headers, footers, and parameterized 
        plugins to your page using a simple callout mechanism.
        ",
        
        #h2 { text="Data Binding" },
        #p{},
        "
        Nitrogen leverages the power of Erlang data structures and pattern matching to 
        enable powerful one-way databinding. Data binding support makes it easy to display repeated
        documents like blog posts or comments.
        ",
        
        #h2 { text="Erlang Power" },
        #p{},
        "
        Nitrogen brings all of the advantages of Erlang to web application development,
        including hot code swapping, stability, and scalability.
        ",
        
        #hr {},
        
        #h2 { text="Technology Stack" },
        #h3 { text="Mac, Linux, Windows, etc." },
        #p{},
        "   
        Any platform that can   run Erlang can also run Nitrogen, including Mac, Linux, Unix, and Windows (among others). 
        Nitrogen does not contain   any platform specific code. 
        ",
        
        #h3 { text="Mochiweb, Yaws, Cowboy, Webmachine, or Inets for Serving" },
        #p{},
        "
        Nitrogen supports some of the most popular Erlang web servers equally: Mochiweb, Yaws, Cowboy, Webmachine, and Inets. 
        Nitrogen abstracts out the server specific code, meaning that you write your application on 
        one http server and seamlessly transfer to a different http server without changing a thing.
        ",
        
        #h3 { text="JQuery/JQuery UI for Javascript and Effects" },
        #p{},
        "
        Nitrogen uses JQuery and the JQuery UI library for client side Javascript and the JQuery Mobile library for mobile development.
        "
    ].

right() ->
    [
        #h2 { text="Documentation" },
        
        "
        <p>
        <a href='/doc/index.html'>Nitrogen documentation</a> is available for 
        browsing online.  It is also included in the source code under <i>/doc/html</i>, and
        is packaged in all binary downloads.
        
        <p>
        Read <a href='/whatsnew'>What's New in Nitrogen 2.x &raquo;</a>
        ",
        
        #h2 { text="About Nitrogen" },

    "
        <p>
        Nitrogen was created in 2008 by <a href='http://rusty.io'>Rusty
        Klophaus</a> (<a href='http://www.twitter.com/rustyio'>@rustyio</a>).
        In June 2011, <a href='http://sigma-star.com/page/jesse/'>Jesse Gumm</a>
        (<a href='http://www.twitter.com/jessegumm'>@jessegumm</a>) took over
        as project lead. It is in active development and is available for use
        under the MIT License.
        ",

        "
        <p>
        Twitter: <a href='http://twitter.com/nitrogenproject'>@nitrogenproject</a>
        ",

        #h2 { text="Official Maintainers"},
        #p{},
        #list { body = [
            lists:map(fun(C) ->
                #listitem { body=format_contrib(C) }
            end,maintainers())
        ] },

        #h2 { text="Thanks!" },

        #p{},
        "Thanks to the many people who have helped make Nitrogen better, including:",

        #p{},
        #list { body=[
            lists:map(fun(C) ->
                #listitem { body=format_contrib(C) }
            end,thanks())
        ] }
    ].

format_contrib({Name}) ->
    Name;
format_contrib({Name,TwitterName}) ->
    [
        Name, 
        " (",twitter(TwitterName),")"
    ];
format_contrib({Name,URL,TwitterName}) ->
    [
        #link{url=URL,text=Name},
        " (",twitter(TwitterName),")"
    ];
format_contrib({Name,URL,TwitterName,Role}) ->
    [
        Role,": ",format_contrib({Name,URL,TwitterName})
    ].


maintainers() ->
    [ 
        {"Jesse Gumm","http://sigma-star.com/page/jesse","jessegumm","Project Leader"}
    ].

thanks() ->
    [
        {"Chris Williams","voodootikigod"},
        {"Joel Reymond","wagerlabs"},
        {"Tom McNulty"},
        {"Martin Scholl","zeit_geist"},
        {"Dave Peticolas"},
        {"Jon Gretar Borgthorsson","jongregar"},
        {"Dan Bravender","dbravender"},
        {"Taavi Talvik"},
        {"Torbjorn Tornkvist","kruskakli"},
        {"Marius A. Eriksen","marius"},
        {"Michael Mullis"},
        {"John Dragos"},
        {"Benjamin Nortier","bjnortier"},
        {"Jay Doane"},
        {"Robert Schonberger"},
        {"Yurii Rashkovskii","yrashk"},
        {"Ville Koivula"},
        {"Manuel Duran Aguete"},
        {"Jan-Felix Wittmann"},
        {"Martin Sivak"},
        {"Mattias Holmlund"},
        {"Loïc Hoguin","lhoguin"},
        {"Justin Kirby"},
        {"Lorant Kurthy"},
        {"Jonas Ådahl"},
        {"Susan Potter","susanpotter"},
        {"Rado Kozmer","rkozmer"},
        {"Tuncer Ayaz"},
        {"Steffan Panning"},
        {"James Pharaoh"},
        {"Sergei Lebedev"},
        {"Milan Svoboda"},
        {"Jenő I. Hajdu"},
        {"Maxim Sothatsky"}
    ].
        

twitter(Username) ->
    #link{url="http://twitter.com/#!/" ++ Username,text=["@",Username]}.

event(_) -> ok.
