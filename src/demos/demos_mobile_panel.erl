% vim: ts=4 sw=4 et
-module (demos_mobile_panel).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demosmobile.html") }.

title() -> "Mobile Panel".

headline() -> "Mobile Panel (toggleable side menu)".

panel() ->
    #mobile_panel{
        id=my_menu,
        position=left,
        display_mode=push,
        theme=a,
        body=[
            #h3{text="Side Menu"},
            "This is a side panel where you can put menus and other options.",
            #br{},
            #button{text="Back to Demos", click=#redirect{url="/demos"}},
            #button{text="Close Panel", click=#toggle_mobile_panel{target=my_menu}},
            linecount:render()
        ]
    }.

body() -> 
    [
        "
        <p>
        A mobile panel is the jQuery mobile way to present a standard
        toggleable mobile side menu. It's a great way to present a menu in a
        standardized way that's familiar to mobile users, while simultaneously
        saving screen real estate.
        <hr>
        ",
        #button{text="Open Panel", click=#toggle_mobile_panel{target=my_menu}}
    ].
