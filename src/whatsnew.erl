% vim: ts=4 sw=4 et
-module (whatsnew).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("grid.html") }.

title() -> "What's New in Nitrogen 2.x".

headline() -> "What's New in Nitrogen 2.x".

layout() ->
    #container_12 { body=[
        #grid_12 { class=header, body=common:header(learn) },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=headline, body=headline() },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=whatsnew_overview, body=top() },
        #grid_clear {},

        #grid_5 { alpha=true, prefix=1, class=pad_right, body=left() },
        #grid_5 { omega=true, suffix=1, body=right() },
        #grid_clear {},

        #grid_12 { body=common:footer() }
    ]}.

top() -> 
    [ 
        " 
        <p> 
        Nitrogen 2.x is the result of hundreds of hours of development
        over the past years, and offers many improvements over Nitrogen
        1.0. Here are the highlights. Behind the scenes, the codebase
        is simpler, cleaner, and better documented, and offers a great
        runway for future versions of Nitrogen.

        <p>
        Happy Hacking!
        "
    ].
        
left() -> 
    [
        #h2 { text="New Elements" },
        "
        <p>
        Nitrogen 2.x includes great new elements, deployment methods, and
        improvements to old elements and actions:

        <p>
        Significantly improved support for jQuery Mobile, including
        mobile-specific elements, allowing you to make adding mobile versions
        of your application much easier.
         
        <p>
        The <code>#api{}</code> element allows you to expose a
        Javascript API that will trigger a postback when called. You
        can pass parameters to the function, and then pattern match on
        the parameters in Erlang. See the <a href='/demos/api'>API
        demo</a>.

        <p>
        The <code>#upload{}</code> element now fires an event when an
        upload has started, and includes the node() on which the
        upload is stored, allowing you to handle uploads in an
        application running on multiple Nitrogen servers. See the <a
        href='/demos/upload'>File Upload Demo</a>. As of version 2.1.0,
        The <code>#upload</code> element now support multiple file uploads,
        progress bars, and even a drag and drop interface.

        <p>
        The <code>#grid{}</code> element provides a Nitrogen interface
        to the <a href='http://960.gs'>960 Grid System</a>.

        <p> 
        The <code>#template{}</code> element now caches the parsed
        template, eliminating unnecessary disk hits.

        <p>
        The <code>#restful_form{}</code> element allows you to easily
        build RESTful forms, allowing for a simpler interface to build
        forms that will safely degrade if the client does not support
        Javascript.

        <p>
        The <code>#wizard</code> element allows you to easily build
        step-by-step multi-page wizards.

        <p>
        Existing elements, including <code>#droppable{}</code>,
        <code>#sortblock{}</code>, and <code>#upload{}</code>, now
        allow you to set delegates to go back to a module other than
        the current page module.

        <p>
        A new <code>#update{}</code> action allows you to wire
        conditional, client-side update commands, or to broadcast
        updates across a comet pool, if desired.  This action is also
        used behind the scenes by APIs such as
        <code>wf:update/2</code>.

        <p>
        The <code>#event{}</code> action now allows you to specify an
        optional <code>keycode</code> attribute on keydown, keyup, and
        keypressed events.

        ",


        #h2 { text="jQuery Selecters" },
        "
        <p>
        Commands such as <code>wf:update/2</code> and
        <code>wf:wire/N</code> now understand jQuery selectors as a
        way of targeting elements. For example, to apply a
        <code>#fade{}</code> action to multiple elements, you can do
        something like this: <code>wf:wire(\"##myelementid > .image\",
        #fade{})</code> to fade all elements tagged with the 'image'
        class underneath the Nitrogen element 'myelementid'. See the
        <a href='/demos/jquerypaths'>jQuery Paths Demo</a> and read
        the <a href='/doc/paths.html'>Nitrogen Paths</a> docs for more
        information.
        ",


        #h2 { text="Simplified Element Hierarchy" },
        "
        <p>
        The previous version of Nitrogen constructed nested elements
        by giving the DOM element an ID of the form
        <i>page_element1_element2_elementN</i>. Nitrogen 2.x instead
        uses CSS classes to refer to elements, so elements have the
        form 'wfid_elementname'. This makes for smaller pages, and
        allows you to more manipulate Nitrogen elements with pure
        Javascript in a predictable way. Additionally, it allowed for
        the creation of a new API commands, <code>wf:replace/N</code>,
        which replaces an element with a new block of elements.
        "
    ].


right() ->
    [
        #h2 { text="Browser-based testing framework!"},
        "
        <p>
        Nitrogen 2.3 introduces a dependency-free testing framework for
        validating your pages, postbacks, actions, and elements all work as
        expected. Create test cases that are instantiated server-side, execute
        something client-side (such as button presses), and validate the result
        server-side.
        <a href='/doc/advanced/testing.html'>View the documentation</a>
        then check out
        <a href='https://github.com/nitrogen/NitrogenProject.com/tree/master/src/tests'>the actual tests used by NitrogenProject.com</a>.",

        #h2 { text="Plugin System" },
        "
        <p>
        Easily add custom elements, actions, and more to your Nitrogen apps
        with Nitrogen's <a href='/docs/plugins.html'>plugin system</a>. You can
        <a href='https://github.com/nitrogen/sample_nitrogen_plugin'>Create your
        own plugin</a>, then add your plugin to our
        <a href='https://github.com/nitrogen/nitrogen/wiki/Nitrogen-Plugins'>List of
        Nitrogen Plugins</a>.",
    
        #h2 { text="Comet Pools" },
        "
        <p>
        Nitrogen now contains functionality called Comet Pools that
        implement much of the plumbing around a multi-user
        application. A Comet Pool links together the Comet processes
        in an application, and can either be 'local' (for the current
        session only) or 'global' (for all sessions). Using the
        <code>wf:send/2</code> command, you can broadcast a message to
        all processes in the pool. Additionally, messages are sent
        when processes join (and optionally leave) the pool.

        See the <a href='/demos/comet2'>chatroom demo</a>.
        ",


        #h2 { text="Handlers" },
        "
        <p>
        Core Nitrogen behavior has been broken out into well-defined,
        pluggable behavior modules called Handlers. Handlers allow you
        to easily substitute your own logic for things like session,
        security, routing, and others. Simply create a module that
        implements one of the existing behaviours, and register it
        with a call to <code>nitrogen:handler/2</code> before calling
        <code>nitrogen:run/0</code>.
        ",


        #h2 { text="Cleaner Interface to HTTP Servers" },
        "
        <p>
        Nitrogen now receives all requests through <a
        href='http://github.com/nitrogen/simple_bridge'>SimpleBridge</a>,
        a layer that unifies the interface to various Erlang HTTP
        servers. The list currently includes Mochiweb, Yaws, Cowboy, 
        Webmachine, and Inets. SimpleBridge is capable of adding websocket
        support even to servers that don't support websockets.

        <p> 
        This makes the Nitrogen codebase cleaner, and allows you to
        run Nitrogen <i>under</i> an existing server in a more
        OTP-compliant way. In contrast, Nitrogen 1.x tried to exert
        much more control over the VM and the HTTP server.

        <p>
        Along with this improvement came the ability to run Nitrogen pages
        under any path, removing the requirement to prefix all Nitrogen paths
        with \"web\", e.g. \"http://hostname/web\".
        ",


        #h2 { text="Better Packaging and Deployment" },
        "
        <p>
        Using <a href='http://github.com/basho/rebar/'>Rebar</a> (an
        Erlang build and packaging tool by <a
        href='http://twitter.com/dizzyd'>Dave Smith</a>), Nitrogen
        now supports a cleaner, completely self-contained project
        structure. There are 'make' commands to build Nitrogen
        projects that run under Mochiweb, Yaws, Webmachine, Cowboy,
        or Inets.        

        <p>
        You can deploy Nitrogen in one of three simple ways:
        <ul>
            <li>
            <b>Full Erlang Releases</b>: Complete and self-contained
            package of Nitrogen which includes a proper OTP structure, start
            scripts, and a complete Erlang installation.
            </li>

            <li>
            <b>Slim Erlang Releases</b>: All the bells and whistles of a full
            Erlang release (start scripts, OTP structure), but is much smaller
            because it doesn't require Erlang to be installed
            </li>

            <li>
            <b>Embeddable Nitrogen:</b>: Easily add a Nitrogen front-end to an
            existing application using the `embed` script.
            </li>
        </ul>

        <p>
        The <code>bin/nitrogen</code> script in the project allows you
        to bring a node up and down in either background or console
        mode.  The <code>bin/dev</code> script allows you to quickly
        generate a page, element, or action, and recompile code on a
        headless server.
        

        <p>
        Additionally, this support is used to provide binary packages
        for popular OSes1. See the <a href='/downloads'>downloads</a> page.
        ",

        
        #h2 { text="More Documentation, New Website" },
        "
        <p>
        As you may have already noticed, <a
        href='/doc/index.html'>Nitrogen Documentation</a> is now much
        more complete, and the website has been overhauled to be more clear and more helpful.
        "
    ].

event(_) -> ok.
