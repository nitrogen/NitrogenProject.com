-module (demos_smart_extensions).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Smart Extensions Demo".

headline() -> "Smart Extensions Demo".

left() -> 
    [
        "
        <p>
		Nitrogen's default routing handler allows you to use 'Smart
		Extensions.' A smart extension lets you provide an different entry
		point into a module that may or may not get pre- and post-processed in
		a standard way based on the extension provided to the filename.  The
		most common smart extension might be appending a .json which might then
		return a .json encoded data structure, complete with the appropriate
		MIME-type set.

		<p>
		This demo uses Nitrogen's built-in JSON Smart Extension which, if the
		request has a .json extension, Nitrogen will call
		<code>json_main()</code>, which is expected to return an Erlang
		proplist. The proplist will automatically be converted to a JSON string
		and the MIME-type will be set to <code>\"application/json\"</code>

		<p>
		Click the link to the right to see it in action.

		<p>
		For more detail, see the <a
		href='/doc/smart_extensions.html'>Documentation for Smart
		Extensions</a>

        ",
        linecount:render()
    ].

right() -> 
	[
		"The following link will simply perform a redirect to:
		<code>",wf:url(),".json</code>",
		#br{},
		#link{
			text="Check out the JSON Version of this page",
			url=[wf:url(),".json"]
		}
	].

json_main() ->
	[
		{favorite_fruit, <<"Apple">>},
		{zip_code, 90210},
		{some_squares, [X*X || X <- lists:seq(1,10)]},
		{helpful_tip, <<"When encoding JSON, you will want strings to be binaries, because a 'normal' Erlang string list will be considered a list by the JSON encoder">>}
	].
