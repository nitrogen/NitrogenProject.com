-module (demos_api).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Javascript API".

headline() -> "Javascript API".

left() -> 
    [
        "
        <p>
        The <code>#api{}</code> element allows you to create a
        Javascript API function on your page that will fire requests
        back to Nitrogen. API postbacks are handled by api_event(Name,
        Tag, Arguments).

        <p>
        The <code>name</code> property specifies the name under which the
        function will be exposed.

        <p>
        The <code>tag</code> property allows you to include an opaque
        value that is passed into the event. It is not exposed to the
        client.

        <p>
        The arguments are an Erlang term that maps to the arguments
        you specified in Javascript. These are true Erlang arguments,
        and can be used for pattern matching. Nitrogen uses <a
        href='http://github.com/rklophaus/BERT-JS'>BERT-JS</a> to
        encode Javascript terms into an Erlang binary.

        <p>
        This demo sets up three Javascript APIs on the page, called
        via the links on the right.
        ",
        linecount:render()
    ].

right() -> 
    wf:wire(#api { name=apiOne, tag=f1 }),
    wf:wire(#api { name=apiTwo, tag=f2 }),
    wf:wire(#api { name=apiThree, tag=f3 }),
    wf:wire(#api { name=apiFour, tag=f4 }),
    [
        #flash{},
        #p{},
        %% In a normal Nitrogen application, these HTML elements would usually be #link{} elements,
        %% however, to demonstrate the fact that the elements work with pure javascript with no
        %% Nitrogen trickery, we're using raw <a> elements with javascript bound directly to them
        "<a href=\"javascript: page.apiOne('Hello Joe!');\">page.apiOne('Hello Joe!')</a><br>"
        "<a href=\"javascript: page.apiTwo({ greeting:'Hello', name:'Mike' });\">page.apiTwo({ greeting:'Hello', name:'Mike' })</a><br>"
        "<a href=\"javascript: page.apiThree(Bert.atom('hello'), Bert.atom('robert'), 12345);\">page.apiThree(Bert.atom('hello'), Bert.atom('robert'), 12345)</a>",
        #br{},
        #textbox{id=name, text="Bruce Banner"},
        "<a href=\"javascript: page.apiFour()\">Send Name = page.apiFour()</a>"
    ].

% Notice the argument pattern matching!	
api_event(apiOne, _, ["Hello Joe!"]) ->
    wf:flash("Hello Joe!");

api_event(apiTwo, _, [[{<<"greeting">>, "Hello"}, {<<"name">>, "Mike"}]]) ->
    wf:flash("Hello Mike!");

api_event(apiThree, _, [hello, robert, 12345]) ->
    wf:flash("Hello Robert!");

api_event(apiFour, _, []) ->
    Name = wf:q(name),
    wf:flash(wf:f("You entered ~p",[Name]));

api_event(A, B, C) ->
    ?PRINT(A), ?PRINT(B), ?PRINT(C).

event(_) -> ok.
