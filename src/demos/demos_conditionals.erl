-module(demos_conditionals).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template{file=common:template_location("demos46.html")}.

title() -> "Conditional Actions".
headline() -> "Conditional Actions".

left() -> 
    [
        "
        The <code>#if_value{}</code> and <code>#if_checked{}</code> actions let you
        make conditional checks and perform subsequent actions based on the
        state of form fields.",
        linecount:render()
    ].

right() -> [
    #fieldset{legend_body=#code{text="#if_value{}"}, body=[
        #label{text="Favorite Orchestra Section"},
        #dropdown{id=section, options=[
            {"", "None"},
            {brass, "Brass"},
            {percussion, "Percussion"},
            {strings, "Strings"},
            {woodwinds, "Woodwinds"}
        ]},
        #br{},
        #button{text="Check for blank", click=[
            #if_value{
                target=section,
                value="",
                actions=#alert{text="The selection is empty"},
                'else'=#alert{text="The selection is not empty"}
            }
        ]},
        #button{text="Check actual value", click=[
            #if_value{
                target=section,
                map=[
                    {brass, #alert{text="Toot your Horn"}},
                    {percussion, #alert{text="Bang the Drum"}},
                    {strings, #alert{text="Twang the Strang"}},
                    {woodwinds, #alert{text="Squeak Away"}}
                ],
                'else'=#alert{text="Stand there and do nothing"}
            }
        ]}
    ]},
    #fieldset{legend_body=#code{text="#if_checked{}"}, body=[
        #checkbox{id=agree_box, text="I agree with some things"},
        #br{},
        #button{text="Tell me the box status", click=[
            #if_checked{
                target=agree_box,
                actions=#alert{text="The box is checked"},
                'else'=#alert{text="The box is not checked"}
            }
        ]}
    ]}
].
