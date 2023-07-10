% vim: ts=4 sw=4 et
-module (demos).
-include_lib ("nitrogen_core/include/wf.hrl").
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
    [
        #h2 { text="Controls and Validation" },
        #p{},
        #link { text="Simple Controls", url="/demos/simplecontrols" }, #br{}, 
        #link { text="Limiting Postbacks with Vessels", url="/demos/vessel"}, #br{},
        #link { text="In-Place Textbox", url="/demos/advancedcontrols1" }, #br{}, 
        #link { text="Google Charts", url="/demos/advancedcontrols2" }, #br{}, 
        #link { text="Mermaid Flowchars and Diagrams", url="/demos/mermaid1" },#br{},
        #link { text="QR Codes", url="/demos/qr" }, #br{}, 
        #link { text="Sparkline", url="/demos/sparkline" },#br{},
        #link { text="Radio Buttons", url="/demos/radio" }, #br{}, 
        #link { text="File Uploading", url="/demos/upload" }, #br{}, 
        #link { text="User Notices", url="/demos/notices" }, #br{}, 
        #link { text="Validation", url="/demos/validation" }, #br{},
        #link { text="Dynamically Removing Validation", url="/demos/clear_validation"}, #br{},
        #link { text="Autocompletion", url="/demos/textbox_autocomplete" }, #br{}, 
        #link { text="Wizard", url="/demos/wizard" },#br{},
        #link { text="RESTful Forms", url="/demos/restful" }, #br{},
        #link { text="HTML and Custom Encoding", url="/demos/htmlencode"},#br{},
        #link { text="Rest API handler", url="/demos/rest"}, #br{},
%        #link { text="Recaptcha", url="/demos/recaptcha"},#br{},

        #h2 { text="Drag, Drop & Sort" },
        #p{},
        #link { text="Drag and Drop", url="/demos/dragdrop" }, #br{}, 
        #link { text="Sorting", url="/demos/sorting1" }, #br{}, 
        #link { text="Nested Sorting", url="/demos/sorting2" }, #br{},

        #h2 { text="Data Binding" },
        #p{},
        #link { text="Simple (List-Based) Binding", url="/demos/binding1" }, #br{}, 
        #link { text="Record-Based Binding", url="/demos/binding2" }, #br{}, 
        #link { text="Key/Value Pair Binding", url="/demos/binding3" }, #br{}, 
        #link { text="Binding With a Transform Function", url="/demos/binding4" }, #br{}
    ].

right() ->
    [
        #h2 { text="Events and Ajax" },
        #p{},
        #link { text="Effects", url="/demos/effects" }, #br{}, 
        #link { text="Postbacks", url="/demos/postback" }, #br{}, 
        #link { text="Postbacks with Form Fields", url="/demos/postback2" }, #br{}, 
        #link { text="Priority Wiring", url="/demos/priority_wiring"}, #br{},
        #link { text="AJAX Updates", url="/demos/ajax" }, #br{}, 
        #link { text="AJAX Replace", url="/demos/replace" }, #br{},
        #link { text="AJAX Remove", url="/demos/remove" }, #br{},
        #link { text="Spinner", url="/demos/spinner" }, #br{},
        #link { text="JQuery Paths", url="/demos/jquerypaths" }, #br{}, 
        #link { text="Javascript API", url="/demos/api" }, #br{},
        #link { text="Mermaid with Postbacks", url="/demos/mermaid2" },#br{},

        #h2 { text="Comet/Asynchronous Calls" },
        #p{},
        #link { text="Counter with Comet", url="/demos/comet1" }, #br{},
        #link { text="Chatroom with Comet", url="/demos/comet2" }, #br{},
        #link { text="Multiple Comets with Graceful Exit", url="/demos/comet3" }, #br{},
        #link { text="Continuations", url="/demos/continuations" }, #br{},
        #link { text="Delayed Content (with some caching)", url="/demos/delay_body"}, #br{},
        #link { text="Progress Bars", url="/demos/progress_bar" },#br{},
        #link { text="Synchronized Panels (using comet)", url="/demos/sync_panel" },#br{},
        #link { text="Mermaid Async Updates", url="/demos/mermaid3" },#br{},
        #link { text="Custom Websocket Functionality", url="/demos/websocket"},#br{},

        #h2 { text="Mobile Integration" },
        #p{},
        #link { text="Side Panel", url="/demos/mobile_panel" },#br{},
        #link { text="Mobile Lists", url="/demos/mobile_list" },#br{},
        #link { text="Mobile Controls with Postbacks", url="/demos/mobile_controls" },#br{},
        #link { text="Mobile Collapsibles", url="/demos/mobile_collapsibles" },#br{},
        #link { text="Dynamically adding Mobile Elements", url="/demos/mobile_controls2" },#br{},

        #h2 { text="Advanced Topics" },
        #p{},
        #link { text="Set Content Type and Response Headers", url="/demos/contenttype" }, #br{},
        #link { text="Smart Extensions (JSON, CSV, etc)", url="/demos/smart_extensions" }, #br{},
        #link { text="Request HTTP Headers", url="/demos/headers" }, #br{},
        #link { text="Security (User management)", url="/demos/security" }, #br{},
        #link { text="State (Page and Session)", url="/demos/state" }
    ].  
