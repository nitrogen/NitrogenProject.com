%% vim: ts=4 sw=4 et
-module (demos_wizard).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Wizard".

headline() -> "Wizard".

left() -> 
    [
        "
        <p> 
        With the #wizard element, you can trivially make multi-step wizards for
        your users.  You just need to define a few Wizard step 'bodies' as if
        it were the body of a Nitrogen element (e.g. #panel).  Then on your
        page, you must define a wizard_event(Tag) function to handle when the
        wizard is completed, and retrieve the values.

        On the right is a demo of a sample 'character creation' process for
        something like an MMO or role-playing game.
        <p>
        ", 
        linecount:render() 
    ].

right() -> 
    Titles = ["Name", "Email", "Race"],
    Steps = [step(1), step(2), step(3)],
    wire_validators(length(Steps)),

    #wizard{
        id=create_character,
        titles=Titles,
        steps=Steps,
        tag=wizard
    }.

wire_validators(NumSteps) ->
    [wire_validators(Step, NumSteps) || Step <- lists:seq(1,NumSteps)].

wire_validators(Step, NumSteps) ->
    %% Get the buttonids for the "next" buttons
    Buttonids = element_wizard:next_button_ids(Step, NumSteps),

    %% Get the required fields from each step
    RequiredFields = step_required(Step),

    %% Wire the validators to the Buttonids for each of the Required Fields
    [wf:defer(Buttonid,Field,#validate{validators=#is_required{text="Required"}})
        || Buttonid <- Buttonids,
           Field <- RequiredFields].

step_required(1) ->
    [name];
step_required(2) ->
    [];
step_required(3) ->
    [race, class].

step(1) ->
    [
        #label{text="Enter your name (required)"},
        #textbox{id=name}
    ];
step(2) -> 
    [
        #label{text="Enter your email address (optional)"},
        #textbox{id=email, type=email}
    ];
step(3) ->
    [
        #label{text="Enter your fantasy race (required)"},
        #dropdown{id=race, options=[
            {"Choose Race",""},
            {"Orc","Orc"},
            {"Human","Human"},
            {"Elf","Elf"},
            {"Robot","Robot"}
        ]},
        #label{text="Enter your class (Required)"},
        #dropdown{id=class, options=[
            {"Choose Class",""},
            {"Wizard","Wizard"},
            {"Fighter","Fighter"},
            {"Thief","Theif"},
            {"Artificial Intelligence Technician","AI Tech"}
        ]}
    ].

%% Summary function for the completion of the Wizard
new_character(Name, Email, Race, Class) ->
    #panel{body=[
        #h3{text="You've created a new character"},
        "Name: ",Name,#br{},
        "Email: ",Email,#br{},
        "Race: ",Race,#br{},
        "Class: ",Class
    ]}.

%% This gets called when the "Finish" button is pressed
wizard_event(wizard) ->
    [Name, Email, Race, Class] = wf:mq([name, email, race, class]),

    %% Replace the wizard element with the "new character summery"
    wf:replace(create_character, new_character(Name, Email, Race, Class)).
