%% vim: ts=4 sw=4 et
-module (demos_postback2).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Postbacks with Forms Fields".

headline() -> "Postbacks with Forms Fields".

left() -> 
    [
        "
        <p>
        Postbacks are the primary way in which data is submitted to the server.
        Field values can be retrieved in postbacks with the use of the
        <code>wf:q/1</code> function call along with its siblings,
        <code>wf:qs/1</code>, <code>wf:mq/1</code>, <code>wf:mqs/1</code>,
        <code>wf:q_pl/1</code>, and <code>wf:qs_pl/1</code>.
        <p>
        Unlike HTML, you don't have to worry about adding
        <code>&lt;form&gt;</code> tags - an element's ID is used.
        ",
        linecount:render()
    ].

right() -> 
    [   
        #label{text="Your Name"},
        #textbox{id=name},
        #br{},

        #label{text="Your Fantasy Race"},
        #dropdown{id=race, options=[
            #option{text="Hobbit"},
            #option{text="Orc"},
            #option{text="Human"},
            #option{text="Goblin"},
            #option{text="Troll"}
        ]},
        #br{},

        #label{text="What weapon(s) do you carry?"},
        #checkbox{id=weapon, value="Sword", text="Sword"},
        #br{},
        #checkbox{id=weapon, value="Axe", text="Axe"},
        #br{},
        #checkbox{id=weapon, value="Dagger", text="Dagger"},
        #br{},
        #checkbox{id=weapon, value="Staff", text="Staff"},
        #br{},

        #button{postback=submit1, text="Submit Method 1"},
        #button{postback=submit2, text="Submit Method 2"},
        #button{postback=submit3, text="Submit Method 3"},
        #br{},
        #panel{id=summary}
    ].  

event(submit1) ->
    %% Here we get the values of the fields name and race with wf:q.
    Name = wf:q(name),
    Race = wf:q(race),

    %% And the values of all checked weapon boxes with wf:qs.
    %% wf:qs is used to get multiple fields with the same name
    Weapons = wf:qs(weapon),
    update_summary(["Method 1:<br>", format_summary(Name, Race, Weapons)]);

event(submit2) ->
    %% Here we shorten the code a little by using wf:mq to get multiple fields.
    [Name, Race] = wf:mq([name, race]),

    %% But we must still use wf:qs to get the selected Weapons
    Weapons = wf:qs(weapon),
    update_summary(["Method 2:<br>", format_summary(Name, Race, Weapons)]);

event(submit3) ->
    %% Here we just use wf:qs_pl to retrieve a bunch of fields into a proplist
    Proplist = wf:qs_pl([name, race, weapon]),

    %% And we'll print the proplist as is, for demonstration purposes
    update_summary(wf:f("Method 3 (proplist):<br><pre>~n~p</pre>", [Proplist])).

format_summary(Name, Race, Weapons) ->
    wf:f("<pre>Name: ~p~nRace: ~p~nWeapons: ~p~n</pre>", [Name, Race, Weapons]).


update_summary(Summary) ->
    wf:update(summary, Summary),
    wf:wire(summary, #effect{effect=highlight}).
