%% vim: ts=4 sw=4 et
-module (demos_simplecontrols).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos433.html" }.

title() -> "Simple Controls".

headline() -> "Simple Controls".

left() ->
    [
    <<"
    <p>
    Nitrogen lets you build HTML using Erlang record syntax. For
    example, a Level-1 Header is specified as:

    <p>
    <pre>#h1 { text=\"My Header\" }</pre>

    <p>
    This page lists some of the simple HTML elements included in
    Nitrogen.
    ">>, 
    linecount:render() 
].

middle() -> 
    [
        #h1 { text="Header 1"},
        #h2 { text="Header 2"},
        #h3 { text="Header 3"},
        #h4 { body=[
            "Header 4 ",
            #image{image="/images/MiniSpaceman.png"},
            " with an image in it"
        ]},
        #p{},
        #hr{},

        #h2 { text="Forms" },
        #p{},
        #label { text="TextBox" },
        #textbox { id=textbox, text="This is a textbox." }, 

        #p{},
        #label { text="Readonly TextBox" },
        #textbox { text="This is a readonly textbox.", readonly=true, next=date_textbox }, 
        #link{ text="Skip Link", click=#alert{text="This textbox will be skipped when tabbing from the readonly textbox to the datepicker textbox"}},

        #p{},
        #label { text="DatePicker Textbox" },
        #datepicker_textbox { id=date_textbox  }, 

        #p{},   
        #label { text="TextArea (tabs are trapped on this textarea)" },
        #textarea { id=textarea, text="This is a textarea.", trap_tabs=true },

        #p{},   
        #label { text="Password Box" },
        #password { id=password, text="Password" },

        #p{},
        #dropdown { id=my_dropdown, options=[
            #option { value="", text="Dropdown" },
            #option { value="opt1", text="Option 1" },
            #option { value="opt2", text="Option 2" },
            #option { value="opt3", text="Option 3" }
        ]},

        #p{},
        #dropdown { id=multiple, multiple=true, options=[
            #option { value="1", text="Multiselect 1" },
            #option { value="2", text="Multiselect 2" },
            #option { value="3", text="Multiselect 3" }
        ]},

        #p{},
        #radiogroup { id=myRadio, body=[
            #radio {text="Option 1", value="1", checked=true }, #br{},
            #radio {text="Option 2", value="2" }, #br{},
            #radio {text="Option 3", value="3" }, #br{},
            #radio {text="Option 4", value="4" }
        ]},

        #p{},
        #checkbox { id=checkbox, value="check1", text="Checkbox 1", checked=true },#br{},
        #checkbox { id=checkbox, value="check2", text="Checkbox 2", checked=false },#br{},
        #checkbox { id=checkbox, value="check3", text="Checkbox 3", checked=true },

        #p{},
        #button { id=button, text="Button", postback=postback },
        #button { id=disabled_button, text="Disabled Button", disabled=true }
    ].

right() -> 
    [
        #p{},
        #h2 { text="Lists" },
        #list { body=[
            #listitem { text="List Item 1" },
            #listitem { text="List Item 2" },
            #listitem { body=#checkbox { text="List Item 3" }}
        ]},

        #list { numbered=true, body=[
            #listitem { text="List Item 1" },
            #listitem { text="List Item 2" },
            #listitem { body=#checkbox { text="List Item 3" }}
        ]},
        #h2 { text="Gravatars"},
        #gravatar{ email="RKlophaus@Gmail.com", size="100", rating="x" }, 
        #p{},
        #gravatar{ email="dan.bravender@test.com", size="100", default="wavatar" }
    ].

event(postback) ->
    wf:wire(#alert{text=wf:q(textbox)}),
    wf:wire(#alert{text=wf:qs(multiple)}).

%% TESTS BELOW

test_main() ->
    wf_test:start(fun tests/0),
    main().

tests() ->
    ?wf_test_auto(textbox, test_textbox()),
    ?wf_test_auto(textarea, test_textarea()),
    ?wf_test_auto(dropdown, test_dropdown()),
    ?wf_test_auto(radio, undefined, fun check_radio/0),
    ?wf_test_auto(checkbox, test_checkbox()),
    ?wf_test_auto(checkbox_changed, test_checkbox_changed()),
    ?wf_test_auto(insert_after_order, test_insert_after_order()),
    ?wf_test_auto(multi_dropdown, test_multidropdown()),
    ?wf_test_auto(multi_dropdown1, test_multidropdown1()),
    ?wf_test_manual(click_postback, test_remove_multi_click_postback()).

test_textbox() ->
    Str = "Some new val",
    {
        fun() -> wf:set(textbox, Str) end,
        fun() -> wf:q(textbox)== Str end
    }.

test_textarea() ->
    Str = "A long-form\nString",
    {
        fun() -> wf:set(textarea, Str) end,
        fun() -> wf:q(textarea)==Str end
    }.

test_dropdown() ->
    {
        fun() -> wf:set(my_dropdown, "opt2") end,
        fun() -> wf:q(my_dropdown) == "opt2" end
    }.

test_checkbox() ->
    {
        undefined,
        fun() -> wf:qs(checkbox) == ["check1","check3"] end
    }.

test_checkbox_changed() ->
    {
        fun() -> wf:set_multiple(checkbox, ["check2", "check3"]) end,
        fun() -> wf:qs(checkbox) == ["check2","check3"] end
    }.

test_insert_after_order() ->
    {
        fun() ->
            wf:insert_after(disabled_button, #textbox{id=order1, text="1"}),
            wf:insert_after(disabled_button, #textbox{id=order1, text="2"}),
            wf:insert_after(disabled_button, #textbox{id=order1, text="3"})
        end,
        fun() ->
            wf:qs(order1) == ["3","2","1"]
        end
    }.

test_multidropdown() ->
    {
        fun() -> wf:set_multiple(multiple, ["1", "2", "3"]) end,
        fun() -> wf:qs(multiple) == ["1","2","3"] end
    }.

test_multidropdown1() ->
    {
        fun() -> wf:set_multiple(multiple, ["2"]) end,
        fun() -> wf:qs(multiple) == ["2"] end
    }.

check_radio() ->
    wf:q(myRadio)=="1".

test_remove_multi_click_postback() ->
    {
        fun() ->
            wf:replace(disabled_button, #button{text="Test Button", id=my_new_button, delegate=wf_test, postback=click_postback}),
            wf:remove(multiple),
            wf:defer(my_new_button, #click{})
        end,
        fun() ->
            wf:qs(multiple)==[]
        end
    }.
