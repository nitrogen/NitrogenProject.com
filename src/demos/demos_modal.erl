-module(demos_modal).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Modal Popups".
headline() -> "Modal Popups".

left() -> 
    [
        "Modals are effectively popup boxes with interfaces, and while Nitrogen
        provides basic modal functionality, it also provides a modal handler,
        which empowers the developer using a front-end framework of their
        choice to create and customize how the modal handler interface is
        defined.",
        linecount:render()
    ].

right() -> [
    #button{text="Open without a postback", click=#modal{text="This is a basic popup"}},
    #br{},
    #button{text="Open from a Postback", postback=open_basic},
    #br{},
    #button{text="Open Modal that has multiple buttons", postback=open_multi},
    #br{},
    #button{text="Open 3 modals, then close the middle one after one second", postback=open_three},
    #button{text="Open a more complex modal", postback=open_complex}
].

process_name(Name) when ?WF_BLANK(Name) ->
    "";
process_name(Name) ->
    ["(currently saved as ",wf:html_encode(Name),")"].

event(open_basic) ->
    wf:wire(#modal{text="This is a modal opened in a postback"});
event(open_multi) ->
    Modal = #modal{
        text="What would you like to do?",
        buttons=[
            {"Open another modal without closing this one", open_basic},
            {"Close this modal and open two modals", open_two}
        ],
        close_text="Close this modal"
    },
    wf:wire(Modal);
event(open_two) ->
    wf:wire([
        #close_modal{},
        #modal{
            title_text="Modal 1 (should be behind)",
            body=[
                "Here is a body for this modal.",
                #br{},
                "It can have both <b>HTML</b> and ",#em{text="Nitrogen Elements"},
                "just like any other body."
            ]
        },
        #modal{title_text="Modal 2", close_text="Close Just this Modal", buttons=[
            #button{text="Close both modals", click=[#close_modal{}, #close_modal{}]}
        ]}
    ]);
event(open_three) ->
    wf:wire([
        #modal{
            id=bottom,
            body=["This is the bottom modal",lists:duplicate(15, #br{})],
            show_close_button=false
        },
        #modal{
            id=middle,
            body=["This is the middle module", lists:duplicate(7, #br{})],
            show_close_button=false
        },
        #modal{
            id=top,
            body=["This is the top modal"],
            buttons=[
                #button{text="Close bottom and top modals", click=[
                    #close_modal{id=bottom},
                    #close_modal{id=top}
                ]}
            ],
            show_close_button=false
        },
        #event{
            type=timer,
            delay=2000,
            actions=[
                #close_modal{id=middle}
            ]
        }
    ]);
event(open_complex) ->
    Name = wf:state(name), %% get name from the page state
    wf:wire([
        #modal{
            title_text="This is a fancier Modal",
            show_close_button=false,
            body=[
                "It has a title, and body with some elements, a mixture of
                different kinds of buttons, and the default close button is
                disabled.",
                #br{},
                #label{body=[
                    "Enter your name",
                    #span{id=current_name, body=process_name(Name)}
                ]},
                #textbox{id=name}
            ],
            buttons=[
                %% standard buttons for modals
                {"Save", save_name},
                {"Save and Close", save_and_close},
                " | ",
                %% Can also be any number of Nitrogen Elements
                #button{
                    id=close_without_save,
                    text="Close without Saving",
                    click=[
                        #show{target=confirm_close},
                        #hide{target=close_without_save}
                    ]
                },
                #span{
                    id=confirm_close,
                    style="display:none", %% hidden with CSS
                    body=[
                        "Really close without saving?",
                        #button{text="Yes, Close", click=#close_modal{}},
                        #button{text="No, Cancel", click=[
                            #hide{target=confirm_close},
                            #show{target=close_without_save}
                        ]}
                    ]
                }
            ]
        }
    ]);
event(save_name) ->
    Name = wf:q(name),
    wf:state(name, Name),
    wf:update(current_name, process_name(Name));
event(save_and_close) ->
    event(save_name), %% this just reuses the logic in event(save_name)
    wf:wire(#close_modal{}).

