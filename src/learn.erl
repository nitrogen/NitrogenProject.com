% vim: sw=4 ts=4 et
-module (learn).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("grid.html") }.

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
        Nitrogen uses an event-driven model built on top of Erlang pattern
        matching. Nitrogen allows you to tag elements with any Erlang term, and
        then act on the tag in server-side code when the user clicks on, hovers
        over, or otherwise interacts with the element.  Handling the event is as
        simple as writing an Erlang function.
        ",

        #h2 { text="Browser-Based Testing"},
        #p{},
        "
        Nitrogen 2.3 introduced a dependency-free testing framework for
        validating your pages, postbacks, actions, and elements all work as
        expected. Create test cases that are instantiated server-side, execute
        something client-side (such as button presses), and validate the result
        server-side.
        <a href='/doc/advanced/testing.html'>View the documentation</a>
        then check out
        <a href='https://github.com/nitrogen/NitrogenProject.com/tree/master/src/tests'>the actual tests used by NitrogenProject.com</a>.",

        #h2 { text="Websockets Galore" },
        #p{},
        "
        Update or replace entire sections of your page asynchronously over
        websockets in a single line of code (with a clean fallback to ajax if
        Websockets are not supported by the proxy or client). Nitrogen lets you
        use the same consistent syntax to both build AND update the page.
        Nitrogen also includes support for long-running processes. By simply
        wrapping your long-running function call with a single line of code,
        you can turn a synchronous function into a long-running asynchronous
        function.
        ",

        #h2{text="Javascript? What is it good for?"},
        #p{},
        "
        With Nitrogen's element construct, you rarely have to shift contexts
        from a server-side mentality (Erlang) to a client-side mentality (HTML,
        Javascript). Nitrogen applications can be built in pure Erlang and only
        rarely require hand-written Javascript or HTML (CSS, unfortunately,
        still needs to be written). Never shift contexts from client to
        server and back again.
        <br>
        Or put more succinctly, <b>\"Whereas Node.js brings the client-side to
        the server, Nitrogen brings the server-side to the client.\"</b>.
        ",

        #h2 { text="Built-In Caching"},
        #p{},
        "
        Use Nitrogen's Cache Handler to locally cache pages or parts of pages to
        'turbo-ize' your pages.  Uses ",#link{new=true, text="NitroCache", url="https://github.com/nitrogen/nitro_cache"},
        " by default, but as with all Nitrogen's handlers, you can write your own
        cache handler to tap into Mnesia, Redis, Memcached, or any other cache system
        you want
        ",
        
        #h2 { text="Complex Interfaces: Drag/Drop/Sort" },
        #p{},
        "
        With Nitrogen, you can let a user interact with your application in
        complex ways.  Nitrogen lets you tag the draggable, droppable, or
        sortable elements, and then respond to the resulting events in
        server-side code as easily as you would respond to a click. 
        ",
        
        #h2 { text="Flexible Templating" },
        #p{},
        "
        Nitrogen includes a simple but powerful template system, allowing you
        to define a consistent style for your application. You can add headers,
        footers, and parameterized plugins to your page using a simple callout
        mechanism.  With Pandoc, make templates from non-HTML sources as well.
        That includes Markdown, LaTeX, RTF, PDF, and more!
        ",
        
        #h2 { text="Data Binding" },
        #p{},
        "
        Nitrogen leverages the power of Erlang data structures and pattern
        matching to enable powerful one-way databinding. Data binding support
        makes it easy to display repeated documents like blog posts or comments.
        ",

        #h2 { text="Infinitely Extendible" },
        #p{},
        "
        With Nitrogen, you can create your own custom complex elements and
        actions, then package them up as Nitrogen Plugins, to easily include in
        other Nitrogen projects without having to copy-and-paste, or share them
        with the world.
        ",
        
        #h2 { text="Erlang Power" },
        #p{},
        "
        Nitrogen brings all of the advantages of Erlang to web application
        development, including hot code swapping, stability, and scalability.
        ",
        
        #hr {},
        
        #h2 { text="Technology Stack" },
        #h3 { text="Mac, Linux, Windows, FreeBSD, Raspberry Pi etc." },
        #p{},
        "   
        Any platform that can run Erlang can also run Nitrogen, including Mac,
        Linux, Unix, and Windows (among others).  Nitrogen does not contain
        any platform specific code. 
        ",
        
        #h3 { text="Mochiweb, Yaws, Cowboy, Webmachine, or Inets for Serving" },
        #p{},
        "
        Nitrogen supports some of the most popular Erlang web servers equally:
        Mochiweb, Yaws, Cowboy, Webmachine, and Inets.  Nitrogen abstracts out
        the server specific code, meaning that you write your application on one
        http server and seamlessly transfer to a different http server without
        changing a thing.
        ",
        
        #h3 { text="JQuery/JQuery UI/JQuery Mobile for Javascript and Effects" },
        #p{},
        "
        Nitrogen uses JQuery and the JQuery UI library for client side
        Javascript.  JQuery Mobile is included for now, but is deprecated and will
        be removed.
        "
    ].

right() ->
    [
        #h2 { text="Documentation" },
        
        "
        <p>
        <a href='/doc/index.html'>Nitrogen documentation</a> is available for 
        browsing online.  It is also included in the source code under <code>/doc</code>, and
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
        as project lead. It is in active development, in use in industry, and is available for use
        under the MIT License.
        ",

        "
        <p>
        Twitter: <a href='http://twitter.com/nitrogenproject'>@nitrogenproject</a>
        ",

        #h2 { text="Official Maintainers"},
        #p{},
        #list { body = [
            [#listitem{body=format_contrib(C)} || C <- maintainers()]
        ] },

        #h2 { text="Thanks!" },

        #p{},
        "Thanks to the many people who have helped make Nitrogen better, including:",

        #p{},
        #list { body=[
            [#listitem{body=format_contrib(C)} || C <- thanks()]
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
        {"Jesse Gumm","http://jesegumm.com","jessegumm","Project Leader"}
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
        {"Maxim Sokhatsky","5HT"},
        {"Roman Shestakov","rshestakov"},
        {"Witeman Zheng"},
        {"Chan Sisowath"},
        {"Florent Gallaire"},
        {"Dmitriy Kargapolov"},
        {"Andrii Zadorozhnii"},
        {"Evan Miller","evmill"},
        {"Alice Blitter Copper"},
        {"Petr Kozorezov"},
        {"Nikolay Garanko"},
        {"Paul Khusainov"},
        {"David N. Welton","davidnwelton"},
        {"Tobias Herre"},
        {"Josh Pyle"},
        {"Niclas Axelsson"},
        {"Evgeny M."},
        {"Boris Resnick"},
        {"Piotr Nosek"},
        {"Stefan Zegenhagen"},
        {"Mehmet Emin Tok"},
        {"Stuart Thackray"},
        {"Amos Oviedo", "fooflare"},
        {"Alex Popov", "seidlitz"},
        {"Aaron Frantisak"},
        {"Lloyd R. Prentice"},
        {"Cameron Frederick", "cammcad"},
        {"Xue Hongwei"},
        {"Steve Vinoski", "stevevinoski"},
        {"Aaron Bieber"},
        {"Tobias Weisenthal"},
        {"Franklin Brauning"},
        {"Olivier Boudeville"},
        {"Alex Ksandra"},
        {"Ramkrishna Kulkarni"},
        {"Meike Hecker"},
        {"Mikael Bylund"},
        {"Alexander Sedov"},
        {"João Henrique Ferreira de Freitas"}

    ].
        

twitter(Username) ->
    #link{url=["http://twitter.com/",Username],text=["@",Username]}.

event(_) -> ok.
