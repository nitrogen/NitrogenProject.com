-module (demos_htmlencode).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "HTML Encoding".

headline() -> "HTML Encoding".

bbcode_encode(RawText) ->
	EncodingFuns = [bbc_b,bbc_i,bbc_u,nl2br],
	Raw2 = wf:html_encode(RawText),
	lists:foldl(fun(Proc,Cur) -> ?MODULE:Proc(Cur) end,Raw2,EncodingFuns).
		
simple_bbcode(BBCodeTag,OpenHTML,CloseHTML,Raw) ->
	re:replace(Raw,"\\[" ++ BBCodeTag ++ "\\](.*?)\\[/" ++ BBCodeTag ++ "\\]",OpenHTML ++ "\\1" ++ CloseHTML,[global,dotall,caseless,{return,list}]).

bbc_i(Raw) ->
	simple_bbcode("i","<i>","</i>",Raw).

bbc_b(Raw) ->
	simple_bbcode("b","<b>","</b>",Raw).

bbc_u(Raw) ->
	simple_bbcode("u","<u>","</u>",Raw).

nl2br(Raw) ->
	re:replace(Raw,"\n","<br />",[global,{return,list}]).

left() -> 
    [
        "
	<p>
	HTML Encoding can be done a handful of different ways, including customized encoding methods.   On some elements, such as <code>#span{}</code>, you can use the <code>html_encode</code> property to encode the text a certain way.

	<p>
	<code>html_encode</code> can be either of <code>true</code>, <code>false</code>, <code>whites</code>, <code>number</code>, or a function of arity 1
        ",
        linecount:render()
    ].

right() -> 
	Raw = "With HTML Tags:

<b>Lorem ipsum</b> dolor sit amet, <i>consectetur adipiscing elit</i>. Proin vestibulum tempor gravida. <u>Aliquam et tellus vel sapien molestie aliquet.</u> Proin eget dui libero.

With BBCode Tags:

[b]Aliquam sollicitudin[/b], nibh eget posuere feugiat, [i]nunc velit pharetra risus[/i], eget laoreet neque metus at nibh.",
[
	#label{text="Text to Encode"},
	#textarea{id=encoding_text,text = Raw,style="width:300px;height:250px"},

	#label{text="HTML Encoding Mode:"},
	#button{text="true",postback=true},
	#button{text="false",postback=false},
	#button{text="whites",postback=whites},
	#button{text="custom(bbcode)",postback=bbcode},
	#br{},
	#button{text="custom(to_lower)",postback=to_lower},
	#button{text="custom(no_spaces)",postback=no_spaces},
	
	#label{id=encoding_label,text="Current encoding: true"},
	#span{id=encoding,text=Raw,html_encode=true}
].

update_encoding(Label,Encoding) ->
	Text = wf:q(encoding_text),
	wf:update(encoding_label,"Current encoding: " ++ Label),
	wf:replace(encoding,#span{id=encoding,text=Text,html_encode=Encoding}).

event(true) ->
	update_encoding("true",true);
event(false) ->
	update_encoding("false",false);
event(whites) ->
	update_encoding("whites",whites);
event(bbcode) ->
	% pass a function in this module
	update_encoding("custom(bbcode)",fun ?MODULE:bbcode_encode/1);
event(to_lower) ->
	% pass a function in another module
	update_encoding("custom(to_lower)",fun string:to_lower/1);
event(no_spaces) ->
	% inline function
	update_encoding("custom(no_spaces)",fun(T) -> [X || X<-T,X /= 32] end).
		
