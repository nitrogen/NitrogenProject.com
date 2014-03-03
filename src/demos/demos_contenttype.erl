-module (demos_contenttype).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> 
	case wf:q(postD) of 
   		undefined ->
   			#template { file="./templates/demos46.html" };
   		_ ->
   		   wf:header("Content-Disposition", "attachement; filename=\"nitrogen.jpeg\""),
   		   demos_contenttype_image:main()
   	end.

title() -> "Content Type".

headline() -> "Content Type".

left() -> 
    [
        "
        <p>
        In a pinch, you can use Nitrogen to dynamically serve content
        other than HTML. The image to the right is served as raw data
        from a Nitrogen module.

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
        [#restful_form{ 
                                          action = "/demos_contenttype",
                                          target = new,
                                          body = [#hidden{id=postD, text = "Post"},
                                                  #restful_submit{text = "Response Header Example"}]
                                         }
    ].
	
event(_) -> ok.
