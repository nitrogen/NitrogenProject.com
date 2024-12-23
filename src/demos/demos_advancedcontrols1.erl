-module(demos_advancedcontrols1).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "In-Place Controls".

headline() -> "In-Place Controls".


left() -> 
    [
        "
		The In-Place controls allow a user to edit a field or value in-place.
        ",
        linecount:render()
    ].

right() ->
    [
        #flash{},
        #h2{text="In-place Text Box and Text Area"},

        #h3{text="In-place Textbox"},
        #inplace_textbox{
            id=textbox1,
            tag=txt1,
            text="Sample Text 1."
        },

        #h3{text="In-place textbox with validation"},
        #inplace_textbox{
            id=textbox2,
            tag=txt2,
            text="Sample Text 2.",
            hover_text="Click change this to a number",
            validators=[#is_integer{text="Must be an integer"}]
        }, 

        #h3{text="In-place textarea (for paragraphs)"},
        #inplace_textarea{
           id=textarea,
           tag=txt3,
           text="Longer Text 3."
        },

        #h2{text="Generalized In-Place Element"},
        #h3{text="In-place element with custom form control"},
        #inplace{
            id=dropdown1,
            text="option 1",
            tag=dd1,
            edit=#dropdown{options=dropdown_options()}
        },

        #h3{text="In-place element with form control and a custom view"},
        #inplace{
            id=dropdown2,
            text="option 1",
            tag=dd2,
            view=#strong{class=label},
            edit=#dropdown{options=dropdown_options()}
        },

        #h3{text="In-place element with a compound view and form controls"},
        #inplace{
            id=dropdown3,
            text="option 1",
            tag=dd3,
            view=[
                #span{style="padding:10px; background: #9f9", body=[
                    #strong{id='##', text='$$', class=label}
                ]}
            ],
            edit=[
                #span{style="padding: 10px; background: #f99", body=[
                    #dropdown{id='##', value='$$', options=dropdown_options()}
                ]}
            ]
        },

        #h3{text="In-place element with a function-based view and controls"},
        #inplace{
            id=color,
            text="FFFFFF",
            tag=color,
            hover_text="Click to edit the individual color components",
            view=fun(ID, Val) ->
                ColorString = "#" ++ Val,
                #span{id=ID, class=label, text=ColorString, style="display:inline-block; padding:5px; background-color: " ++ ColorString}
            end,
            edit=fun(ID, Val) ->
                RedID = wf:temp_id(),
                GreenID = wf:temp_id(),
                BlueID = wf:temp_id(),
                [R1,R2,G1,G2,B1,B2] = Val,
                R = [R1,R2],
                G = [G1,G2],
                B = [B1,B2],
                UpdateJS = wf:f("var rgb = objs('~s').val() + objs('~s').val() + objs('~s').val();
                                 objs('~s').val(rgb);", [RedID, GreenID, BlueID, ID]),
                Action = #event{actions=UpdateJS},

                BaseDD = #dropdown{options=hex_options(), actions=[
                    Action#event{type=change},
                    Action#event{type=blur},
                    Action#event{type=click}
                ]},

                [
                    #hidden{id=ID, text=Val},
                    #table{rows=[
                        #tablerow{cells=[
                            #tablecell{text="Red"},
                            #tablecell{body=BaseDD#dropdown{id=RedID, value=R, class=red_dd}}
                        ]},
                         #tablerow{cells=[
                            #tablecell{text="Green"},
                            #tablecell{body=BaseDD#dropdown{id=GreenID, value=G, class=green_dd}}
                        ]},
                        #tablerow{cells=[
                            #tablecell{text="Blue"},
                            #tablecell{body=BaseDD#dropdown{id=BlueID, value=B, class=blue_dd}}
                        ]}
                    ]}
                ]
            end
        }
    ].

hex_options() ->
    ["00", "11", "22", "33", "44", "55", "66", "77",
     "88", "99", "AA", "BB", "CC", "DD", "EE", "FF"].

dropdown_options() ->
    [
        {"Option 1", "Option 1"},
        {"Option 2", "Option 2"},
        {"Option 3", "Option 3"},
        {"Option 4", "Option 4"}
    ].

inplace_textbox_event(Tag, Value) ->
	%% Notify the user of what was entered
	Msg = wf:f("You entered: '~s' into the inplace control tagged '~s'", [Value, Tag]),
	wf:flash(Msg),

	%% Return the new value to be displayed.
	Value.

inplace_textarea_event(Tag, Value) ->
	%% Notify the user of what was entered
	Msg = wf:f("You entered: '~s' into the inplace control tagged '~s'", [Value, Tag]),
	wf:flash(Msg),

	%% Return the new value to be displayed.
	Value.

inplace_event(Tag, Value) ->
    Msg = wf:f("You entered: '~s' into the inplace control tagged '~s'", [Value, Tag]),
    wf:flash(Msg),
    Value.
