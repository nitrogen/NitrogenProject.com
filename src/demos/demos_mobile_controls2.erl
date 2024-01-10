% vim: ts=4 sw=4 et
-module (demos_mobile_controls2).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demosmobile.html") }.

title() -> "Dynamically Adding Mobile Controls".

headline() -> title().

body() ->
    Tempbuttonid = wf:temp_id(),
    [
        "
        <p>
        Some extra steps are necessary when dynamically loading jQuery mobile elements. In particular, after you're finished creating your jquery mobile elements in a postback, you'll want to wire the following javascript to the page: <code><pre>wf:defer(\"$('#pagediv').trigger('create')\")</pre></code>

        <hr>
        ",
        #button{text="Add a Toggle", postback={add,toggle}},
        #button{text="Add a Range", postback={add, range}},
        #button{text="Add a Textbox", postback={add, textbox}},
        #button{id=Tempbuttonid, text="Replace this Button with another", postback={replace, Tempbuttonid}},

        #panel{id=new_elements,body=[
            "New elements will go below:",
            #br{}
        ]},
        #link{url="/demos",text="Back to Demos"},
        linecount:render()
    ].


textbox() ->
    #textbox{placeholder="Some Placeholder Text"}.

toggle() ->
    #mobile_toggle{
        html_id=toggle,
        on_text="On",
        off_text="Off",
        selected=off,
        width=200
    }.

range() ->
    #range{min=10,max=100,step=10,value=50}.

event({replace, Tempbuttonid}) ->
    Newbuttonid = wf:temp_id(),
    Label = "New Button ID = " ++ Newbuttonid,
    Button = #button{id=Newbuttonid, text=Label, postback={replace, Tempbuttonid}},
    wf:defer("$('#pagediv').trigger('create'); Nitrogen.$fix_jquery_mobile_ids()"),
    wf:replace(Tempbuttonid, Button);
    
event({add, Type}) ->
    NewElement = case Type of
        textbox -> textbox();
        toggle -> toggle();
        range -> range()
    end,

    %% This line will tell Nitrogen that we've created new mobile elements and we need to create them all
    wf:defer("$('#pagediv').trigger('create')"),

    wf:insert_bottom(new_elements, [#br{}, NewElement]).
