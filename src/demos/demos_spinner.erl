-module (demos_spinner).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Spinner".

headline() -> "Spinner".

left() -> 
    [
        "
        <p>
        Spinner is automatically shown on a start of AJAX event and hidden
        when it finishes.

        <p>
        In this demo, the 'Load' and 'Reset' buttons are wired to update the
        content of the textbox. The update is deliberately delayed to demonstrate
        spinner function.
        ", 
        linecount:render() 
    ].

right() -> 
    [
	#textbox { id=theMessage, next=theButton },
	#button { id=theResetButton, text="Reset", postback=reset },
	#button { id=theLoadButton, text="Load", postback=load },
	#p{},
	#spinner{}
    ].

event(load) ->
    timer:sleep(2000),
    wf:set(theMessage, "Hello World!"),
    ok;	

event(reset) ->
    timer:sleep(1000),
    wf:set(theMessage, ""),
    ok;

event(_) -> ok.
