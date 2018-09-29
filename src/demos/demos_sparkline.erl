%% vim: ts=4 sw=4 et
-module (demos_sparkline).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos433.html" }.

title() -> "Sparkline".

headline() -> "Sparkline".

left() -> 
    [
        "
        <p> 
        Sparklines are basically small graphs and can be used quite effectively into dashboard-type interfaces or alongside text.
        <p>
        ", 
        linecount:render() 
    ].

middle() ->
    Data = [?WF_RAND_UNIFORM(100) || _ <- lists:seq(1,15)],
    PieData = [?WF_RAND_UNIFORM(100) || _ <- lists:seq(1,3)],
    [
        #h2{text="Simple"},
        #h3{text="(No options specified)"},
        #label{text="Line"},
        #sparkline{values=Data},
        #br{},

        #label{text="Bar"},
        #sparkline{type=bar, values=Data},
        #br{},

        #label{text="Tristate"},
        #sparkline{type=tristate, values=[format_tristate(N) || N <- Data]},
        #br{},

        #label{text="Discrete"},
        #sparkline{type=discrete, values=Data},
        #br{},
        
        #label{text="Bullet"},
        #sparkline{type=bullet, values=Data},
        #br{},

        #label{text="Pie Chart"},
        #sparkline{type=pie, values=PieData},
        #br{},

        #label{text="Box"},
        #sparkline{type=box, values=Data},
        #br{}
    ].

format_tristate(N) when N < 33 -> -1;
format_tristate(N) when N > 66 -> 1;
format_tristate(_) -> 0.

right() ->
    Data = [?WF_RAND_UNIFORM(100) || _ <- lists:seq(1,15)],
    [
        #h2{text="Advanced"},
        #h3{text="(customized with options)"},

        #label{text="Custom Size"},
        #sparkline{values=Data, options=[{width, "200px"}, {"height","40px"}]},
        #br{},

        #label{text="Custom Bar Width and Color"},
        #sparkline{values=Data, type=bar, options=[{barWidth, 10}, {barColor, "#0f0"}]},
        #br{},

        #label{text="Custom Threshold Value and Color"},
        #sparkline{values=Data, type=discrete, options=[{thresholdValue, 40}, {thresholdColor, "#f00"}]},
        #br{},

        #label{text="Pie Chart Size and Border"},
        #sparkline{values=Data, type=pie, options=[{width, "100px"},{height, "100px"}, {borderWidth, 2}, {borderColor, "#000"}]}
    ].
