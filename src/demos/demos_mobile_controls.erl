% vim: ts=4 sw=4 et
-module (demos_mobile_controls).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demosmobile.html" }.

title() -> "Mobile Controls and Postbacks".

headline() -> "Mobile Controls and Postbacks".

body() -> 
    [
        "
        <p>
        Nitrogen provides a few mobile-formatted controls, but those that aren't provided can usually be accomplished by setting the 'data-role' attribute, and in some cases, as long as the jQuery mobile library is loaded, it'll handle everything on its own.
        <hr>
        ",
        #label{text="Text Box (no special formatting)"},
        #textbox{id=text},
        #br{},

        #label{text="Search Box (set type=search)"},
        #textbox{id=search, type=search, data_fields=[{icon,search}]},
        #br{},

        #label{text="URL Box (set type=url)"},
        #textbox{id=url, type=url},
        #br{},

        #label{text="Mobile Slider (Toggle)"},
        #mobile_toggle{
            on_text="On",
            off_text="Off",
            selected=off,
            id=slider,
            width=200
        },
        #br{},
        #label{text="Range Element"},
        #range{id=range,min=10,max=100,step=10,value=50},
        #br{},
        #button{text="Initiate Postback", postback=click},
        #panel{id=result, style="display:none"},
        #link{url="/demos",text="Back to Demos"},
        linecount:render()
    ].

event(click) ->
    [Text, Search, Url, Slider, Range] = wf:mq([text, search, url, slider, range]),
    Result = wf:f("Results of postback:~nText: \"~s\",~nSearch: \"~s\",~nURL: \"~s\",~nSlider: \"~s\",~nRange: \"~s\"",[Text, Search, Url, Slider, Range]),
    wf:update(result,wf:html_encode(Result,whites)),
    wf:wire(result,#slide_down{}).
