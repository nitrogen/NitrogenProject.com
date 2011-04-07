-module (demos_grid).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> 
    #template { file="./templates/demos46.html" }.

title() -> "Grid Elements for 960.gs".

headline() -> "Grid Elements".

left() -> 
    [
        "
        <p>
        Nitrogen allows you to define layout elements using the 
        960 grid system
        ",
        linecount:render()
    ].


right() -> 
    [
        #grid_6 { class=example, body=[
            #grid_6 { class=headline, alpha=true, omega=true, body="This is a header of grid size 6" },
            #grid_1 { body="Grid1", suffix=5 },
            #grid_2 { prefix=1, body="Grid2" },
            #grid_3 { prefix=2, body="Grid3" },
            #grid_4 { prefix=3, body="Grid4" },
            #grid_5 { prefix=4, body="Grid5" },
            #grid_6 { prefix=5, body="Grid6" },
            #grid_1 { push=1, body="Push1" },
            #grid_1 { pull=1, body="Pull1" },
            #grid_clear {},
            #grid_2 { class=sidebar, alpha=true, body="Sidebar G2" },
            #grid_4 { class=content, omega=true, body="Content G4" },
            #grid_3 { class=footer, prefix=3, omega=true, body="Footer G3 P3" }
        ] }
    ].

event(_) -> ok.
