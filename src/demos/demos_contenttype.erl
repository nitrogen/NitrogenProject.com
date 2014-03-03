-module (demos_contenttype).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> 
    #template { file="./templates/demos46.html" }.

title() -> "Content Type and Headers".

headline() -> "Content Type and Headers".

left() -> 
    [
        "
        <p>
        In a pinch, you can use Nitrogen to dynamically serve content
        other than HTML. The image to the right is served as raw data
        from a Nitrogen module.

        <p>
        Also, this shows how you can use the <code>wf:header/2</code>
        function to dynamically set the headers, and in this case, use the
        \"Content-Disposition\" header to make the browser download a file
        rather than displaying it in the browser directly.

        <p>
        View the <a
        href=viewsource?module=demos_contenttype_image>source code</a>
        of demos_contenttype_image.erl to see how it's done.
        ",
        linecount:render()
    ].

right() -> 
    [
        #image { image="/demos/contenttype/image" },
        #link { text="Download Image", url="/demos/contenttype/image?download=1" }
    ].
    
event(_) -> ok.
