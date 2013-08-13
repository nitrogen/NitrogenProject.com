% vim: ts=4 sw=4 et
-module (demos_mobile_collapsibles).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demosmobile.html" }.

title() -> "Mobile Collapsibles".

headline() -> "Mobile Collapsibles".

body() -> 
    [
        "
        <p>
        Collapsibles allow us to add toggleable sections to your application. A Collpasible Set allows us to add a set of collapsibles that will cause the opening of one collapsible to close another collapsible within the same set.
        <hr>
        ",
        #mobile_collapsible{
            header_text="An isolated collapsible",
            content_text="Here is the content of this isolated collapsible"
        },
        #mobile_collapsible{
            header_theme=a,
            content_theme=e,
            header_text="Another isolated collapsible",
            content_body=[
                #label{text="Some label"},
                #textbox{text="Some textbox"},
                #button{text="Some Button"}
            ]
        },

        #mobile_collapsible_set{
            header_theme=b,
            content_theme=b,
            body=[
                #mobile_collapsible{
                    header_text="Want to view the source?",
                    content_body=[
                        linecount:render()
                    ]
                },
                #mobile_collapsible{
                    header_theme=e,
                    content_theme=e,
                    header_text="A different-colored section header",
                    content_body="Here is some text with a different color-theme"
                },
                #mobile_collapsible{
                    header_text="Link back to demos is here",
                    content_body=[
                        #link{text="Back to Demos",url="/demos"}
                    ]
                }
            ]
        }
    ].
