%% vim: ts=4 sw=4 et
-module(demos_websocket).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Simple Controls".

headline() -> "Simple Controls".

left() ->
    wf:wire("Nitrogen.$register_ws_handler(\"alerter\", function(data) { alert(\"Received from Server: \" + data) })"),
    [
    <<"
    <p>
    While the core of Nitrogen's communication runs off a websocket,
    you're able to use that websocket to send your own messages and handle your own messages in a much lighter-weight than the standard Nitrogen events.
    <p>

    If your Nitrogen socket is connected, you can send messages with:

    <pre>Nitrogen.$ws_send(\"my message\");</pre>

    Handle the messages on your page module by defining a function:

    <pre>ws_message_event(Data, _Bridge) ->
    wf:info(\"Received: ~p\", Data);
    {reply, {text, [\"Received: \",Data]}}.</pre>

    And then, in your page's javascript, you can handle the responses by registering a function. Here we'll register a handler we'll call \"alerter\" that will pop up a javascript alert box:

    <pre>Nitrogen.$register_ws_handler(\"alerter\", function(data) {
    alert(\"This was sent from the server over the websocket: \" + data);
})</pre>
">>, 
    linecount:render() 
].

right() -> 
    [
        #button{
            postback=normal,
            text="This is a normal postback button"
        },
        #hr{},
        #textbox{
            id=sender,
            text="My message to send",
            size=40
        },
        #button{
           text="Send Websocket Message",
           click="Nitrogen.$ws_send(obj('sender').value)"
        }  
    ].

event(normal) ->
    ?PRINT(wf:qs(multiple)),
    wf:wire(#alert{text="You clicked the normal postback button"}).

%% We don't care about the bridge value, but if we did, we could pull it.
ws_message_event({text, Data}, _Bridge) ->
    wf:info("Received: ~p",[Data]),
    Response = ["This was sent: ", Data],
    {reply, {text, Response}}.

