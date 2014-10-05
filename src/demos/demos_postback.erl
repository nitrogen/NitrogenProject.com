%% vim: ts=4 sw=4 et
-module (demos_postback).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Postbacks".

headline() -> "Postbacks".

left() -> 
    [
        "
        <p>
        Postbacks allow you to build interactive web applications. You
        can wire a Nitrogen element to listen to any Javascript
        event. When the event fires, the element will post back to the
        <code>event/1</code> function on the calling page (or
        alternatively some delegate module) with the element's tag.
        This allows you to use the power of Erlang pattern matching to
        build an event-driven application.

        Nitrogen postbacks will execute sequentially in the order they are
        pressed, meaning if one postback is taking a while, clicking another
        postback will be delayed until the first postback completes.
        ",
        linecount:render()
    ].

right() -> 
    [   
        #button { id=test, text="Press Me", postback=button_pressed },

        #p{},
        #link { text="Click Me", postback=link_clicked },

        #p{},   
        #label { text="Press enter in the textbox." },
        #textbox { id=my_textbox, text="This is a message...", postback=textbox_enterkey },

        #p{},
        #checkbox { id=my_checkbox, text="Toggle Me", postback=checkbox_clicked },

        #p{},   
        #dropdown { id=my_dropdown, postback=dropdown_changed, options=[
                #option { text="Option 1" },
                #option { text="Option 2" },
                #option { text="Option 3" }
        ]},

        #p{},   
        #span { text="Mouse Over Me", actions=#event { type=mouseover, postback=span_mousedover } },
        #p{},
        #button { text="Click for a slow postback (takes 5 seconds)", postback=slow}
    ].  


%% event/1 is just a function that will end up being called with the term used
%% in an element's postback. You can then pattern match against the passed
%% postback term.
%%
%% Here, we capture the a few of the postbacks that need some special treatment
%% than the rest.

event(textbox_enterkey) ->
    %% Get the value of the textbox with id=my_textbox
    Val = wf:q(my_textbox),

    %% wf:f is a shortcut for io_lib:format/2 (with a few enhancements)
    Msg = wf:f("Enter pressed in textbox with value '~s'", [Val]),
    wf:wire(#alert { text=Msg });

event(checkbox_clicked) ->
    %% Get the value of the checkbox (if it was checked)
    Val = wf:q(my_checkbox),

    %% Checkboxes can be a little *odd* in HTML, a checked box will return a
    %% value (which defaults to \"on\"), and an unchecked box is not submitted
    %% at all, and so will return 'undefined' from wf:q
    Msg = wf:f("Checkbox clicked with value: ~p", [Val]),
    wf:wire(#alert { text=Msg });

event(dropdown_changed) ->
    %% Get the value of the dropdown with id=my_dropdown
    Val = wf:q(my_dropdown),
    Msg = wf:f("Dropdown changed to value: '~s'", [Val]),
    wf:wire(#alert { text=Msg });

event(slow) ->
    timer:sleep(5000),
    wf:wire(#alert { text="Slow process completed! I hope you're still here!" });

event(EventInfo) ->
    %% General postback info that we're going to catch here.  Just capture the
    %% postback and alert the user what the postback tag was
    Msg = wf:f("Postback received with value: ~p", [EventInfo]),
    wf:wire(#alert { text=Msg }).
