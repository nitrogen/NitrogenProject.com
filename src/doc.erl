% vim: sw=4 ts=4 et
-module (doc).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/grid.html" }.

title() -> "Documentation".

layout() -> 
    #container_12 { body=[
        common:github_fork(),
        #grid_12 { alpha=true, omega=true, class=header, body=common:header(docs) },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=headline, body=headline() },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, body=[
            documentation_menu()
        ]},
        #grid_clear{},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, body=[
            documentation()
        ]},

        #grid_clear{},

        #grid_12 { alpha=true, omega=true, body=common:footer() }
    ]}.

headline() -> 
    "Documentation".

get_page() ->
    case wf:path_info() of
        "" ->
            wf:coalesce([wf:q(file), ""]);
        File ->
            File
    end.

doc_page() ->
    Raw = get_page(),
    FileToLoad = case re:run(Raw, "([\\w\\.]+?)(?:\\.md|\\.html)?$", [{capture, all_but_first, list}]) of
        {match, [File]} ->
            File ++ ".md";
        _ ->
            "index.md"
    end,
    FileWithPath = internal_path(FileToLoad),
    case filelib:is_file(FileWithPath) of
        true ->
            FileToLoad;
        false ->
            undefined
    end.

internal_path(File) ->
    filename:join(["static","doc",File]).

github_path(File) ->
    "https://github.com/nitrogen/nitrogen_core/blob/master/doc/markdown/" ++ File.

documentation_menu() ->
    #template{
        file="static/doc/header.md",
        from_type=gfm
    }.

documentation() ->
    case doc_page() of
        undefined ->
            wf:status_code(404),
            "<h1>Documentation Page Not Found</h1>";
        Page ->
            [
                #template{
                    file=internal_path(Page),
                    from_type=gfm
                },
                #br{},#br{},
                #link{url=github_path(Page), body="View Source or Submit Corrections for this Documentation Page"}
            ]
    end.

    
