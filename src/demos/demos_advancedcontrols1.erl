-module(demos_advancedcontrols1).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "In-Place Controls".

headline() -> "In-Place Controls".


left() -> 
    [
        "
		The In-Place textbox and textareas allow a user to edit a field or
		value in-place.
        ",
        linecount:render()
    ].

right() -> [

	#flash{},

	#inplace_textbox { id=textbox1, tag=txt1, text="Sample Text 1." }, 
	
	#p{},			
	#inplace_textbox { id=textbox2,  tag=txt2, text="Sample Text 2." }, 
	
	#p{},			
	#inplace_textarea { id=textarea, tag=txt3, text="Longer Text 3." }
].
	
inplace_textbox_event(Tag, Value) ->
	%% Notify the user of what was entered
	Msg = wf:f("You entered: '~s' into the inplace control tagged '~s'", [Value, Tag]),
	wf:flash(Msg),

	%% Return the new value to be displayed.
	Value.
