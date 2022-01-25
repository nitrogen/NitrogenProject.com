-module (demos_restful).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "RESTful Forms".

headline() -> "RESTful Forms".

left() -> 
    [
        "
        <p>
        The <code>#restful_form{}</code> an official Nitrogen way
        to use standard HTML Forms without having to hand-write the
        HTML Forms.
        <p>
        By default, the Nitrogen <code>id</code> of the element will be
        used as the HTML <code>name</code> attribute inside RESTful forms,
        however, this can be overwritten by specifying the <code>html_name</code>
        attribute.",
        linecount:render()
    ].

right() -> 
    case wf:q(submit) of
        undefined -> form();
        _ -> form_results()
    end.


form() ->
    [
        #restful_form{action="/demos/restful", method=get, body=[
            #label{text="Your Name"},
            #textbox{id=your_name},
            #p{},
            #label{text="Where do you live?"},
            #dropdown{id=planet_name, html_name=your_home, options=[    %% your_name will be the name of the field submitted
                #option{text="Alpha Centuri"},
                #option{text="Earth"},
                #option{text="Mars"}
            ]},
            #p{},
            #restful_submit{html_name=submit,text="Submit"},
            #restful_reset{text="Reset"}
        ]}
    ].

form_results() ->
    [Name,Home] = wf:mq([your_name,your_home]),
    FullResults = wf:params(),
    [
        #table{rows=[
            #tablerow{cells=[
                #tablecell{text="Name:"},
                #tablecell{text=Name}
            ]},
            #tablerow{cells=[
                #tablecell{text="Home:"},
                #tablecell{text=Home}
            ]},
            #tablerow{cells=[
                #tablecell{text="Full POST:"},
                #tablecell{body=#pre{text=wf:f("~p",[FullResults]),html_encode=whites}}
            ]}
        ]},
        #link{url="/demos/restful", text="Back to the form"}
    ].

event(_) -> ok.
