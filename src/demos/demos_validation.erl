% vim: ts=4 sw=4 et
-module (demos_validation).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Validation".

headline() -> "Validation". 

left() -> 
    [
        <<"
        <p>
        Nitrogen allows you to declare validation roles on elements
        that are fired during a postback using the
        <code>#validate{}</code> action.
        
        <p>
        The form to the right requires a valid name and email address,
        matching passwords of at least 6 characters, and validating that an
        entered number is an integer, and is between a specified range.

        <p>
        It also demonstrates attaching a validation message to a different
        Nitrogen Element (in this case attaching the validator for the 'number'
        textbox to the continue button)
        ">>,
        linecount:render()
    ].

right() -> 
    Body = [
        #label { text="Name" },
        #textbox { id=nameTextBox, next=emailTextBox },

        #p{},
        #label { text="Email Address" },
        #textbox { id=emailTextBox, next=passwordTextBox },

        #p{},   
        #label { text="Password" },
        #password { id=passwordTextBox, next=confirmTextBox },

        #p{},   
        #label { text="Confirm" },
        #password { id=confirmTextBox, next=otherTextBox },

        #p{},   
        #label { text="Other (no validation)" },
        #textbox { id=otherTextBox, next=numberTextBox },

        #p{},
        #label { text="Enter a number from 1 to 10: " },
        #textbox { id=numberTextBox, next=continueButton },

        #p{},   
        #button { id=continueButton, text="Continue", postback=continue }
    ],

    wf:wire(continueButton, nameTextBox, #validate { validators=[
        #is_required { text="Required." },
        #custom { text="Must start with 'Rusty'.", tag=some_tag, function=fun custom_validator/2 }
    ]}),

    wf:wire(continueButton, emailTextBox, #validate { validators=[
        #is_required { text="Required." },
        #is_email { text="Enter a valid email address." }
    ]}),

    wf:wire(continueButton, passwordTextBox, #validate { validators=[
        #is_required { text="Required." },
        #min_length { length=6, text="Password must be at least 6 characters long." }
    ]}),

    wf:wire(continueButton, confirmTextBox, #validate { validators=[
        #is_required { text="Required." },
        #confirm_password { password=passwordTextBox, text="Passwords must match." }
    ]}),
    
    wf:wire(continueButton, numberTextBox, #validate { attach_to=continueButton, validators=[
        #is_required { text="A Number is Required (note this is attached to the button instead)"},
        #is_integer { text="This must be a number between 1 and 10", min=1, max=10}
    ]}),

    Body.

event(continue) ->
    Name = wf:q(nameTextBox),
    Message = wf:f("Welcome ~s! Your information is valid.", [Name]),
    wf:flash(Message),
    ok;

event(_) -> ok.

custom_validator(_Tag, Value) ->
    Value1 = string:to_lower(Value),
    case Value1 of 
        "rusty" ++ _ -> true;
        _ -> false
    end.
