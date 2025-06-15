-module(demos_modal).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Modal Popups".
headline() -> "Modal Popups".

left() -> 
    [
        "Modals are effectively popup boxes with interfaces.",
        linecount:render()
    ].

right() -> [
    #button{text="Open from a Postback", postback=open_modal},
    #br{},
    #button{text="Open as Local Script", click=#modal{text="This is a basic popup"}},
    #br{}
].

event(open_modal) ->
    wf:wire(#modal{text="This is a modal opened in a postback"}).
