%% vim: ts=4 sw=4 et
-module (demos_priority_wiring).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Priority Wiring".

headline() -> "Priority Wiring".

left() -> 
    [
        "
        <p>
        For convenience, sometimes you need to control the order in which
        events are wired to the page.  The usual examples are when you wish to
        add validators or do something to new elements after they are added to the
        page.  Nitrogen provides 3 priority levels for these kinds of events:",

        #list{numbered=true,body=[
            #listitem{body="<b>eager</b>: Eager events are executed first and are called with <code>wf:eager</code>"},
            #listitem{body="<b>normal</b>: Normal events are executed second and are called with <code>wf:wire</code>"},
            #listitem{body="<b>defer</b>: Deferred events are executed last and are called with <code>wf:defer</code>"}
        ]},
        "
        The demonstration on the right will show you how you can control the
        order of execution on the <b>client</b> for events wired from the
        <b>server</b>.
        ",
        linecount:render()
    ].

lines() ->
    [
        "Final report of the commercial starship Nostromo, third officer reporting.",
        "The other members of the crew, Kane, Lambert, Parker, Brett, Ash and Captain Dallas, are dead.",
        "Cargo and ship destroyed.",
        "I should reach the frontier in about six weeks.",
        "This is Ripley, last survivor of the Nostromo, signing off."
    ].

right() -> 
    Lines = lines(),
    [
        #table{rows=[
            #tablerow{cells=[
                #tableheader{text="Priority"},
                #tableheader{text="Line"}
            ]},
            [draw_line_row(Line) || Line <- Lines]
        ]},
        #button{text="Re-order the lines below according to priority", postback=reorder},
        #panel{id=reordered_lines}
    ].

draw_line_row(Line) ->
    #tablerow{cells=[
        #tablecell{body=[
            #dropdown{id=priority, value="normal", options=[
                #option{text="Eager (wf:eager)", value="eager"},
                #option{text="Normal (wf:wire)", value="normal"},
                #option{text="Defer (wf:defer)", value="defer"}
            ]}
        ]},
        #tablecell{body=[
            #textbox{id=line, size=60, text=Line}
        ]}
    ]}.

atomized_priority("eager") -> eager;
atomized_priority("normal") -> wire;
atomized_priority("defer") -> defer.

event(reorder) ->
    %% First, let's clear the target panel. We want to make sure this is done
    %% "eagerly" so that anything that might have priority=eager doesn't get
    %% rendered first, then wiped out.
    wf:update(eager, reordered_lines, ""),

    %% mqs = (Multiple Queries = so return a list of all elements with
    %% id=priority, and a list of all elements with priority=line).
    %% This is equivilant to:
    %%
    %%   Priority=wf:qs(priority),
    %%   Lines=wf:qs(line)
    %%
    %% But is put here for demonstration purposes.
    [Priorities, Lines] = wf:mqs([priority, line]),

    %% Now, let's pair up the priorities with the lines
    PriorityLines = lists:zip(Priorities, Lines),

    %% And wire each combination to the page with the specified priority
    lists:foreach(fun({Priority, Line}) ->

        %% Convert the text priority to the function name we're going to call
        AtomPriority = atomized_priority(Priority),

        FormattedLine = #panel{body=[
            #strong{text=[Priority,": "]},
            #span{text=Line}
        ]},

        wf:AtomPriority(reordered_lines, #update{type=insert_bottom, elements=FormattedLine})
        %% This would more typically be done like follows:
        %%
        %%   wf:insert_bottom(WirePriority, reordered_lines, FormattedLine)
        %%
        %% But for the purposees of this demonstration, we want to demonstrate wf:eager vs wf:wire vs wf:defer
        %%
        %% Further, this could also be rendered as follows:
        %%
        %%    wf:priority_wire(AtomPriority, reordered_lines, #update{type=insert_bottom, elements=FormattedLine})
        
    end, PriorityLines),

    %% Finally, when all wiring is complete, let's highlight the target div
    wf:defer(reordered_lines, #effect{effect=highlight, speed=1000, options=[{color, "#ffff00"}]}).
