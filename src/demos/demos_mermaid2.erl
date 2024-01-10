%% vim: ts=4 sw=4 et
-module (demos_mermaid2).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Mermaid with Postbacks".

headline() -> "Mermaid with Postbacks".

left() -> 
    [
        "
        <p>
        Postbacks can be used to update Mermaid graph.
        
        <p>
        Click on <code>Render</code> button to render a new graph.
        The code from the textbox will be send to Mermaid.
        ",
        linecount:render()
    ].

right() ->
    Data = "graph TB\na-->b",
    DataNew = "graph TB\nb-->a",

    [   
        #p{},   
        #label { text="Write the mermaid graph or press button." },
        #textarea { id=textarea, text=DataNew },

        #p{},
        #button { id=test, text="Render", postback=button_pressed },

        #p{},
        #mermaid{id=demoMermaid, code=Data}
    ].  

event(button_pressed) ->
    Data = wf:q(textarea),

    wf:set(demoMermaid, Data),

    ok.
