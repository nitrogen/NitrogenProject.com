% vim: ts=4 sw=4 et
-module(demos).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("grid.html") }.

title() -> "Demos".

layout() -> 
    #container_12 { body=[
        common:github_fork(),
        #grid_12 { alpha=true, omega=true, class=header, body=common:header(demos) },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=headline, body=headline() },
        #grid_clear {},

        #grid_4 { alpha=true, prefix=1, body=left(), class=pad_right },
        #grid_3 { body=middle() },
        #grid_3 { omega=true, suffix=1, body=right() },
        #grid_clear {},

        #grid_12 { alpha=true, omega=true, body=common:footer() }
    ]}.

headline() -> 
    "Demos".

left() -> 
    [
        "
        Click a link on the right to see Nitrogen in action.  
        <p>
        Each demo is a separate Erlang module.  You can view the
        source code of the module using the 'View Module Source' link
        on the left side of the page.

        "
    ].


middle() -> 
    Demos = [
        {"Controls and Validation", [
            {"Simple Controls","/demos/simplecontrols"},
            {"Limiting Postbacks with Vessels","/demos/vessel"},
            {"Quick Forms","/demos/quickform"},
            {"In-Place Textbox","/demos/advancedcontrols1"},
            {"Radio Buttons","/demos/radio"},
            {"File Uploading","/demos/upload"},
            {"User Notices","/demos/notices"},
            {"Validation","/demos/validation"},
            {"Dynamically Removing Validation","/demos/clear_validation"},
            {"Autocompletion","/demos/textbox_autocomplete"},
            {"Wizard","/demos/wizard"},
            {"RESTful Forms","/demos/restful"},
            {"HTML and Custom Encoding","/demos/htmlencode"}
            %{"Recaptcha", "/demos/recaptcha"},
        ]},
        {"Charts and Graphs", [
            {"Google Charts","/demos/advancedcontrols2"},
            {"Mermaid Flowcharts and Diagrams","/demos/mermaid1"},
            {"Mermaid with Postbacks","/demos/mermaid2"},
            {"Mermaid Async Updates","/demos/mermaid3"},
            {"QR Codes","/demos/qr"},
            {"Sparkline","/demos/sparkline"}
        ]},
        {"Drag, Drop & Sort", [
            {"Drag and Drop","/demos/dragdrop"},
            {"Sorting","/demos/sorting1"},
            {"Nested Sorting","/demos/sorting2"}
        ]},
        {"Data Binding", [
            {"Simple (List-Based) Binding","/demos/binding1"},
            {"Record-Based Binding","/demos/binding2"},
            {"Key/Value Pair Binding","/demos/binding3"},
            {"Binding With a Transform Function","/demos/binding4"}
        ]}
    ],
    draw_demos(Demos).

right() ->
    Demos = [
        {"Events and Ajax",[
            {"Effects","/demos/effects" },
            {"Postbacks","/demos/postback" },
            {"Postbacks with Form Fields","/demos/postback2" },
            {"Priority Wiring","/demos/priority_wiring"},
            {"Javascript Conditional Events","/demos/conditionals"},
            {"AJAX Updates","/demos/ajax" },
            {"AJAX Replace","/demos/replace" },
            {"AJAX Remove","/demos/remove" },
            {"Spinner","/demos/spinner" },
            {"JQuery Paths","/demos/jquerypaths" },
            {"Javascript API","/demos/api" },
            {"Modal Popups","/demos/modal" }
        ]},

        {"Comet/Asynchronous Calls",[
            {"Counter with Comet","/demos/comet1" },
            {"Chatroom with Comet","/demos/comet2" },
            {"Multiple Comets with Graceful Exit","/demos/comet3" },
            {"Continuations","/demos/continuations" },
            {"Delayed Content (with some caching)","/demos/delay_body"},
            {"Progress Bars","/demos/progress_bar" },
            {"Synchronized Panels (using comet)","/demos/sync_panel" },
            {"Custom Websocket Functionality","/demos/websocket"}
        ]},
    
        %{"Mobile Integration",[
        %    {"Side Panel", "/demos/mobile_panel"},
        %    {"Mobile Lists","/demos/mobile_list"},
        %    {"Mobile Controls with Postbacks","/demos/mobile_controls"},
        %    {"Mobile Collapsibles","/demos/mobile_collapsibles"},
        %    {"Dynamically adding Mobile Elements","/demos/mobile_controls2"}
        %]},
        {"Advanced Topics",[
            {"Optimized Coalescing with lazy_coalesce", "/demos/coalesce"},
            {"Set Content Type and Response Headers","/demos/contenttype" },
            {"Rest API Handler","/demos/rest"},
            {"Smart Extensions (JSON, CSV, etc)","/demos/smart_extensions" },
            {"Request HTTP Headers","/demos/headers" },
            {"Security (User management)","/demos/security" },
            {"State (Page and Session)","/demos/state"}
        ]}
    ],
    draw_demos(Demos).


draw_demos(Demos) ->
    [draw_demo_section(Sec) || Sec <- Demos].

draw_demo_section({Title, Links}) ->
    [
        #h2{text=Title},
        draw_demo_links(Links)
    ].

draw_demo_links(Links) ->
    RenderedLinks = [draw_demo_link(Link) || Link <- Links],
    wf:join(RenderedLinks, #br{}).

draw_demo_link({Text, Url}) ->
    #link{text=Text, url=Url}.
