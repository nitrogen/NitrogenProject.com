%% vim: ts=4 sw=4 et
-module (demos_mermaid3).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Mermaid Flowchars and Diagrams with Async Updates".

headline() -> "Mermaid with Async Updates".

left() -> 
    [
        "
        <p>
        This page demonstrates how to make a simple Mermaid updating
        the graph via Comet.

        ",
        linecount:render()
    ].

right() ->
    Pets = #{"Dogs" => 386, "Cats" => 85, "Rats" => 15},
    Data = mermaid_pie(Pets),
    Body = [
        #span { text="Graph updated via Comet: " },
        #mermaid{id=myPie, code=Data}
    ],
    
    wf:comet(fun() -> background_update(myPie, Pets) end),
    Body.

event(_) -> ok.

background_update(ControlID, Pets) ->
    % Sleep for a second, then update
    timer:sleep(1000),

    Graph = mermaid_pie(Pets),

    % Update the control. Use wf:set to call the correct javascript function
    wf:set(ControlID, Graph),

    % wf:comet_flush() is only needed because we are looping. Otherwise,
    % we could just let the function complete.
    wf:flush(),

    NPets = update_pets(Pets),
    
	% Loop. This process will automatically be killed once the page stops
	% requesting the output that it generates.
	%
	% Using ?MODULE before the function call will ensure that this process
	% survives code reloads.

    ?MODULE:background_update(ControlID, NPets).

mermaid_pie(#{"Dogs" := Dogs, "Cats" := Cats, "Rats" := Rats }) ->
    wf:f("pie title Pets adopted by volunteers\n\"Dogs\" : ~w\n\"Cats\" : ~w\n\"Rata\" : ~w",
                  [Dogs, Cats, Rats]).

update_pets(#{"Dogs" := Dogs}) when Dogs =< 0 ->
    #{"Dogs" => 400, "Cats" => 50, "Rats" => 1};

update_pets(#{"Dogs" := Dogs, "Cats" := Cats, "Rats" := Rats }) ->
    #{"Dogs" => Dogs - 10, "Cats" => Cats + 1, "Rats" => Rats + 1}.