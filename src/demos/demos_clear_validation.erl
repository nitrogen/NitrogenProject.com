%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (demos_clear_validation).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./templates/demos46.html" }.

title() -> "Dynamically Removing Validation".

headline() -> title().

left() ->
    [
        "
        <p>
        Due to the heavily dynamic nature of Nitrogen, we provide a
        convenient mechanism for deactivating validators that have been added
        to form controls. Without this functionality, you're bound to run into
        javascript errors when form fields get dynamically removed from the
        page that have previously had validators added to them.

        <p>
        The form to the right initially requires entering some text into the
        'name' field and the dropdown box. However, you can use the buttons
        to disable individual validators, or disable all validators, as well as
        providing a method for re-attaching the validators after they've been
        removed
        "
    ].
          

right() -> 
    attach_validators(),
    [
        #flash{},
        #label{text="Enter a Name"},
        #textbox{id=name,text=""},
        #br{},
        #label{text="Choose an option"},
        #dropdown{id=dd,options=[
            #option{value="",text=""},
            #option{value=opt1,text="Option 1"},
            #option{value=opt2,text="Option 2"}
        ]},
        #br{},#br{},
        #button{id=button,postback=click,text="Submit"},
        #hr{},
        #button{id=clear_all,postback=clear,text="Clear All Validators"},
        #button{id=clear_text,postback={clear,name},text="Clear Name Validator"},
        #button{id=clear_dd,postback={clear,dd},text="Clear Dropdown Validator"},
        #br{},#br{},
        #button{id=attach,postback=attach,text="Re-attach Validators"}
    ].

attach_validators() ->
    wf:wire(button,name,#validate{validators=[
        #is_required{}
    ]}),
    wf:wire(button,dd,#validate{validators=[
        #is_required{}
    ]}).

event(click) ->
    wf:flash("Postback Successful!");
event(attach) ->
    attach_validators(),
    wf:flash("Validators Attached");
event({clear,Field}) ->
    wf:wire(#clear_validation{validation_target=Field}),
    wf:flash(wf:f("Validator removed from '~s'",[Field]));
event(clear) ->
    wf:wire(#clear_validation{}),
    wf:flash("All Validators Cleared").

