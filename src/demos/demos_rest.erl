-module (demos_rest).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

-define(API_PATH, "/demos/rest/api").
-define(API_MODULE, demos_rest_api).

main() -> #template { file=common:template_location("demos46.html") }.
	
title() -> "Rest handler example".

headline() -> "HTTP REST Handler".

left() -> 
    [
        "<p>
        Nitrogen allows the creation of HTTP REST API endpoints.

        <p> 
        It is possible to use various HTTP methods to request and recieves
        responses through Nitrogen. Methods are provided for decoding &
        encoding JSON (with <code>wf:json_encode/1</code> and
        <code>wf:json_decode/1</code>. Though you can use whichever JSON
        library you prefer.

        <p>
        You can use curl or any your favorite HTTP client such as postman.
        "
       , 
       linecount:render(),

       linecount:render(["View Source for ", ?API_PATH], ?API_MODULE)
    ].

right() -> 
    HostHeader = wf:header(host),
    APIEndpoint = "http://" ++ HostHeader ++ ?API_PATH,
    DataToEncode = #{
        name => <<"m3rl1n">>,
        city => <<"London">>
    },
    QueryString = wf:to_qs(DataToEncode),
    PostJson = wf:json_encode(DataToEncode),
    [
        "You can test this using the path ",
        #code{body=?API_PATH}, " (this is the ",#code{body=wf:to_list(?API_MODULE)}," module)",
        " via these methods:",
        #br{},
        #h3{body="GET"},
        #code{body=["curl -X GET \"",APIEndpoint,"?",QueryString,"\""]},
        #br{},
        #br{},
        #h3{body="POST"},
        #code{body=["curl -X POST \"",APIEndpoint,"\" -d '",PostJson,"'"]},
        #br{},
        #br{},
        "You can send similar posts request using your favorite HTTP clients
        such as Postman."
    ].

event(_) -> ok.


