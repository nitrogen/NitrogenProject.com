%% vim: ts=4 sw=4 et
-module (demos_upload).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "File Upload".
headline() -> "File Upload".


left() -> 
    [
        <<"
        <p>
        The <code>#upload{}</code> element allows a user to upload one or more
        files into a scratch directory.

        <p>
        The element fires events when the upload starts and when the
        upload completes, and passes back original filename, the name
        of the file on disk, and the node to which the file was
        uploaded.
        ">>,
        #flash {},
        linecount:render()
    ].

right() -> 
    [

        #p{},
        #h3 { text="Upload Example #1" },
        #p{},
        <<"Upload example with modified button text. The default text is
        'Upload'.">>,
        #p{},
        #upload{
            tag=example1,
            button_text="Upload File"
        },
        #hr{},

        #h3 { text="Upload Example #2" },
        #p{},
        <<"This example hides the upload button. When the user selects a file it
        will automatically start uploading.">>,
        #p{},
        #upload{
            tag=example2,
            show_button=false
        },
        #hr{},

        #h3 { text="Upload Example #3" },
        #p{},
        <<"This example is enabled for multiple file uploads and has modified all
        the button texts.">>,
        #p{},
        #upload{
            tag=example3,
            multiple=true,
            file_text="Click me to browse for files on your computer",
            button_text="Upload the files you selected"
        },
        #hr{},

        #h3{ text="Upload Example #4" },
        #p{},
        <<"This example is drag and drop enabled for browsers that support it
        (Chrome, Firefox).">>,
        #p{},
        #upload{
            tag=example4,
            show_button=true,
            droppable=true
        },
        #hr{},

        #h3{ text="Upload Example #5" },
        #p{},
        <<"This example is drag and drop enabled for browsers that support it
        (Chrome, Firefox). Further it's enabled for multiple files, but does
        not show an overall progress bar">>,
        #p{},
        #upload{
            tag=example5,
            show_button=false,
            multiple=true,
            droppable=true,
            overall_progress=false
        }
    ].

event(_) -> ok.

start_upload_event(Tag) ->
    wf:flash(wf:f("Upload started with tag (~p)", [Tag])).

finish_upload_event(_Tag, undefined, _, _) -> 
    wf:flash("Please select a file."),
    ok;

finish_upload_event(Tag, FileName, LocalFileName, Node) ->
    FileSize = filelib:file_size(LocalFileName),
    wf:flash(wf:f("Uploaded file (~p): ~s (~p bytes) to ~s on node ~s.", [Tag, FileName, FileSize, LocalFileName, Node])),
    ok.
