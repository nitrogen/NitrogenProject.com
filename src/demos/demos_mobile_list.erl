% vim: ts=4 sw=4 et
-module (demos_mobile_list).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demosmobile.html" }.

title() -> "Mobile List".

headline() -> "Mobile List".

body() -> 
    [
        "
        <p>
        A mobile list is a standard way to present a list of things in a mobile
        friendly format. Typically, this might be used as a menu of sorts (say
        a list of friends in a social application, or a list of threads in a
        mobile-friendly forum).
        <hr>
        ",
        #mobile_list{
            theme=a,
            inset=false,
            body=[
                #mobile_list_divider{text="This is a divider"},
                #mobile_listitem{text="Not Clickable"},
                #mobile_listitem{text="Also not clickable"},
                #mobile_list_divider{text="These are clickable"},
                #mobile_listitem{body=[
                    #link{
                        text="Load this page again",
                        %% tempid just to make sure the URL is different
                        url=["/demos/mobile_list?_=" ++ wf:temp_id()],
                        mobile_target=true
                    }
                ]},
                #mobile_listitem{body=[
                    #link{
                        text="Go to mobile panel demo",
                        url="/demos/mobile_panel",
                        mobile_target=true
                    }
                ]},
                #mobile_listitem{theme=e,body=linecount:render()},
                #mobile_listitem{theme=a,body=[
                    #link{
                        text="Go back to demos",
                        url="/demos",
                        mobile_target=false
                    }
                ]}
            ]
        }
    ].
