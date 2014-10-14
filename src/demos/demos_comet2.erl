-module (demos_comet2).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Comet Chatroom".

headline() -> "Comet Chatroom".

left() -> 
    [
        "
        <p>
        This page demonstrates how to make a simple chatroom using
        Comet pools. A Comet pool provides the plumbing for creating
        multi-user applications in Nitrogen.

        <p>
        Each comet pool has an Erlang term identifier, and can either
        be <i>local</i>, meaning that it only applies to one user, or
        <i>global</i> meaning that it applies to all users.

        <p>
        Here, we create a global Comet pool titled 'chatroom'. Anyone
        who visits this page connects to the same comet pool.  Upon
        clicking the 'Send' button, a message is broadcast to all
        comet processes that have registered in the pool. The comet process
        then updates the page with the new message.
        
        <p>
        Try opening a few different browser windows and chatting with
        yourself.
        ",
        linecount:render()
    ].

right() ->
	#panel{id=chatwrapper, body=[
		#span {text="Name of chatroom"},
		#textbox{id=chatname, text="Chatroom"},

		#button{postback=start_chat, text="Start Chat"}
	]}.

right_step2(Chatroom) ->
    [
        #span { text="Your chatroom name: " }, 
        #textbox { id=userNameTextBox, text="Anonymous", style="width: 100px;", next=messageTextBox },

        #p{},
        #panel { id=chatHistory, class=chat_history },

        #p{},
        #textbox { id=messageTextBox, style="width: 70%;", next=sendButton },
        #button { id=sendButton, text="Send", postback={chat,Chatroom} }
    ].

event(start_chat) ->
	Chatroom = wf:q(chatname),
	wf:replace(chatwrapper, right_step2(Chatroom)),
	start_chat(Chatroom);

event({chat,Chatroom}) ->
    Username = wf:q(userNameTextBox),
    Message = wf:q(messageTextBox),
    wf:send_global(Chatroom, {message, Username, Message}),
    wf:wire("obj('messageTextBox').focus(); obj('messageTextBox').select();");

event({reconnect, Chatroom}) ->
	wf:insert_bottom(chatHistory, [#p{}, #span{text=["Reconnecting to ",Chatroom], class=message }]),
	start_chat(Chatroom).

start_chat(Chatroom) ->
	wf:wire(#comet{
		scope=global,
		pool=Chatroom,
		function=fun() -> start_chat_loop(Chatroom) end,
		reconnect_actions=[
			#event{postback={reconnect, Chatroom}}
		]
	}).

start_chat_loop(Chatroom) ->
	add_message(["Connected to ",Chatroom]),
	chat_loop().

chat_loop() -> 
    receive 
        'INIT' ->
            %% The init message is sent to the first process in a comet pool.
			add_message("You are the only person in the chat room.");
        {message, Username, MsgText} ->
			add_message({Username, MsgText})
    end,
    chat_loop().

add_message(Message) ->
	FormattedTerms = format_message(Message),
	wf:insert_bottom(chatHistory, FormattedTerms),
	wf:wire("obj('chatHistory').scrollTop = obj('chatHistory').scrollHeight;"),
	wf:flush().

format_message({Username, MsgText}) ->
	[
		#p{},
		#span { text=Username, class=username }, ": ",
		#span { text=MsgText, class=message }
	];
format_message(MsgText) ->
	[
		#p{},
		#span { text=MsgText, class=message }
	].

