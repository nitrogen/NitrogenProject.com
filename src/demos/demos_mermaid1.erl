%% vim: ts=4 sw=4 et
-module (demos_mermaid1).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos433.html") }.

title() -> "Mermaid Flowchars and Diagrams".

headline() -> "Mermaid Flowchars and Diagrams".

left() -> 
    [
        "
        <p> 
        Create diagram and flowchart from text.

        <p>
        Additional options can be configured to change diagram behaviours.
        ", 
        linecount:render() 
    ].

middle() ->
    Data = "graph TB\na-->b",

    [
        #h2{text="Simple"},
        #h3{text="(No options specified)"},
        #label{text="Flowchart"},
        #mermaid{code=Data},
        #br{}
    ].

right() ->
    OptionsSequence = {sequence, [{showSequenceNumbers, true}, {width, 100}, {height, 100}]},
    Options = [{theme,"dark"}],
    Data = "sequenceDiagram\nAlice->John: Hello John, how are you?\n\nNote over Alice,John: A typical interaction",
    [
        #h2{text="Advanced"},
        #h3{text="(customized with options)"},

        #label{text="Custom Size"},
        #mermaid{code=Data, options=Options, diagram_options=OptionsSequence},
        #br{}
    ].
