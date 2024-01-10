-module (demos_advancedcontrols2).
-include_lib ("nitrogen_core/include/wf.hrl").
-include_lib ("nitrogen_core/include/google_chart.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Google Charts".
headline() -> "Google Charts".

left() -> 
    [
        "
        <p>
        The <code>#google_chart{}</code> element lets you place a line
        chart, bar chart, or pie chart on the page using the Google
        Chart API.
        ",
        linecount:render()
    ].

right() -> [
    #h2 { text="Line Chart" },
    #google_chart {
		id=line_chart,
        title="Line Chart", width=400, height=200, grid_x=25, grid_y=33,			
        axes=[
            #chart_axis { position=bottom, labels=["First", "Second", "Third", "Fourth"] },
            #chart_axis { position=left, labels=["Good", "Better", "Best"] }
        ],
        data=[
            #chart_data { legend="Data 1", color="FF9900", line_width=3, line_length=1, blank_length=3, 
                values=[10, 20, 30, 20, 30, 40, 40, 50, 60] 
            },
            #chart_data { legend="Data 2", color="2768A9", line_width=5, 
                values=[20, 50, 70, 90, 70, 40, 10,  1,  8] 
            }				
        ]
    },

    #h2 { text="Bar Chart" },
    #google_chart {
		id=bar_chart,
        title="Bar Chart",	type=grouped_vertical_bar, width=400, height=200,	bar_space=0, bar_group_space=10,
        axes=[
            #chart_axis { position=left, labels=["Good", "Better", "Best"] }
        ],
        data=[
            #chart_data { legend="Data 1", color="FF9900", 
                values=[30, 40, 40, 50, 60] 
            },
            #chart_data { legend="Data 2", color="2768A9",  
                values=[70, 40, 10,  1,  8] 
            }				
        ]
    },


    #h2 { text="3D Pie Chart" },
    #google_chart {
		id=pie_chart,
        title="3D Pie Chart",
        type=pie3d,
        width=400, height=200,

        axes=[
            #chart_axis { position=bottom, labels=["First", "Second", "Third", "Fourth"] }
        ],
        data=[
            #chart_data { legend="Data 1", 
                values=[10, 20, 30, 20] 
            }
        ]
    }
].

event(_) -> ok.
