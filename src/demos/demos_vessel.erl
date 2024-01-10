%% vim: ts=4 sw=4 et
-module (demos_vessel).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos433.html") }.

title() -> "Postback Vessels".

headline() -> "Postback Vessels".

left() ->
    [
    <<"
    Nitrogen has a concept called 'vessels' that works similarly to HTML forms
    by limiting the which elements get submitted to the server in a postback.
    <p>
    Simply specify the <code>vessel</code> attribute on any action or element
    that triggers a postback to limit the postback elements to the items
    contained in the named vessel.
    <p>
    If the <code>vessel</code> attribute is an atom, then it will be treated as
    an element ID.  If the attribute is anything else (string, binary, iolist),
    then it will be treated as a CSS/jQuery selector.    
    ">>, 
    linecount:render() 
].

middle() -> 
    [
        #panel{id=foods, body=[
            #label{text="Favorite Foods"},
            #textbox{id=food1, text="Red curry with chicken"},
            #br{},
            #textbox{id=food2, text="Any spicy sushi rolls"},
            #br{},
            #textbox{id=food3, text="Cheeseburger with an egg"}
        ]},
        #panel{id=languages, body=[
            #label{text="What programming languages do you know?"},
            #textbox{id=lang1, text="Erlang"},
            #br{},
            #textbox{id=lang2, text="Javascript"},
            #br{},
            #textbox{id=lang3, text="Dart"}
        ]},
        #panel{id=games, body=[
            #label{text="Favorite Board Games"},
            #textbox{id=game1, text="Orleans"},
            #br{},
            #textbox{id=game2, text="Ghost Stories"},
            #br{},
            #textbox{id=game3, text="Champions of Midgard"}
        ]}
    ].

right() -> 
    [
        #h3{text="Submit what in a postback?"},
        #button{
            text="All",
            postback=submit
        },
        #button{
            text="Foods",
            postback=submit,
            vessel=foods
        },
        #button{
            text="Languages",
            postback=submit,
            vessel=languages
        },
        #button{
            text="Games",
            postback=submit,
            vessel=games
        },
        #button{
            text="Languages + Games",
            postback=submit,
            vessel=[languages, games]
        },
        #button{
            text="Games + Food",
            postback=submit,
            vessel=[games, foods]
        },
        #button{
            text="Food + Games",
            postback=submit,
            vessel=[foods, games]
        },
        #button{
            text="Submit None",
            postback=submit,
            vessel=no_element_with_this_id
        },
        #hr{},
        #panel{id=results}
    ].

event(submit) ->
    Fields = [food1, food2, food3,
              lang1, lang2, lang3,
              game1, game2, game3],
    Body = table_results(Fields),
    wf:update(results, Body).

table_results(Fields) ->
    #table{rows=[
        [results_row(Field) || Field <- Fields]
    ]}.

results_row(Field) ->
    Value = wf:q(Field),
    #tablerow{cells=[
        #tablecell{text=[Field,"="]},
        #tablecell{text=wf:f("~p",[Value])}
    ]}.
