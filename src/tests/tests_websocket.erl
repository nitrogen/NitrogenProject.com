%% vim: ft=nitrogen
-module(tests_websocket).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

-define(OPTS, [{timeout, 2000}, {delay, 200}]).

main() ->
    wf_test:start(fun tests/0),
    #template{file="priv/templates/grid.html"}.

layout() ->
    [
        "<script>
            function arraybuffer_to_array(data) {
                var view = new Uint8Array(data);
                console.log(data);
                var array = [];
                for(var i=0; i < view.byteLength; i++) {
                    array[i]=view[i];
                }
                console.log(array);
                return array;
            }
        </script>",

        #h1{text="Websocket Test"},
        #panel{id=log}
    ].

tests() ->
    ?wf_test_js(test_ws_text, test_ws_text("test1")),
    ?wf_test_js(test_ws_multiple_handlers, test_ws_text("test2")),
    ?wf_test_js(test_delete_handler, test_delete_handler("test1")),
    ?wf_test_js(test_ws_decode, test_ws_encoded()).

test_ws_text(Testname) ->
    Setup = 
        fun() ->
            log("Registering New Handler: ~p",[Testname]),
            wf:wire("Nitrogen.$register_ws_handler(\"" ++ Testname ++ "\", function(data) {
                if(data=='" ++ Testname ++ "') {
                    Nitrogen.$last_test_data=data;
                    return true;
                }else{
                    return false;
                }
            })"),
            wf:wire("Nitrogen.$ws_send('" ++ Testname ++ "')"),
            log("Sending: ~p", [Testname])

        end,
    JS = "return Nitrogen.$last_test_data",
    Assertion =
        fun([V]) ->
            log("Received: ~p",[V]),
            V == Testname
        end,
    {Setup, JS, Assertion, ?OPTS}.

test_ws_encoded() ->
    Testname = "encoded",
    Orig = {original_term, "Something"},
    EncodedData = term_to_binary(Orig),
    Bytearray = js_bytearray(EncodedData),
    Setup = 
        fun() ->
            log("Registering New Handler: ~p",[Testname]),
            wf:wire("Nitrogen.$register_ws_handler(\"" ++ Testname ++ "\", function(data) {
                if(typeof data=='object') {
                    Nitrogen.$last_test_data=data;
                    return true;
                }else{
                    return false;
                }
            })"),
            wf:wire(["Nitrogen.$console_log(",Bytearray,")"]),
            wf:wire(["Nitrogen.$ws_send(",Bytearray,")"]),
            log("Sending: ~p", [Bytearray])
        end,
    JS = "return arraybuffer_to_array(Nitrogen.$last_test_data);",
    Assertion =
        fun([V]) ->
            log("Received: ~p",[V]),
            Bin = list_to_binary(V),
            Decoded = binary_to_term(Bin, [safe]),

            %% This should be comparing against the {re_encoded, ...}
            Decoded == {re_encoded, Orig}
        end,
    {Setup, JS, Assertion, ?OPTS}.

js_bytearray(Bin) ->
    List = binary_to_list(Bin),
    ListStr = [integer_to_list(I) || I <- List],
    iolist_to_binary(["new Uint8Array([", lists:join(",", ListStr), "])"]).

test_delete_handler(HandlerName) ->
    %% Tell the client to remove the existing ws_handler as teh setup
    Setup = fun() ->
        log("Deleting Handler: ~p",[HandlerName]),
        wf:wire("Nitrogen.$unregister_ws_handler(\"" ++ HandlerName ++ "\")")
    end,
    
    %% Get how many handlers there are and pass it as the value to the #api call
    JS = "return Nitrogen.$websocket_handlers.length",
    
    %% Verify on the server that the number of handlers is 1
    Assertion = fun([V]) ->
        log("Number of remaining WS handlers: ~p",[V]),
        V==1
    end,
    {Setup, JS, Assertion}.


log(Msg, Vars) ->
    Msg2 = wf:f(Msg, Vars),
    log(Msg2).

log(Msg) ->
    wf:insert_bottom(log, [#panel{actions=#effect{effect=highlight}, text=Msg}]).

ws_message_event({text, Msg}, _Bridge) ->
    {reply, {text, Msg}};
ws_message_event({decoded, Decoded}, _Bridge) ->
    Encoded = term_to_binary({re_encoded, Decoded}),
    {reply, {binary, Encoded}}.

