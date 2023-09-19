-module (demos_rest).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.
	
title() -> "Rest handler example".

headline() -> "HTTP REST Handler".

left() -> 
    [
        "
        <p>
        Nitrogen allows the creation of HTTP REST API endpoints.

        <p> 
        It is possible to use various HTTP methods to request and recieves responses through Nitrogen. Methods are provided for decoding & encoding json. Though you can use which ever JSON client you prefair.

        <p>
        You can use curl or any your favorite HHTP client such as postman..
        "
       , 
       linecount:render()
    ].

right() -> 
    [
        "
        <p>
        You can test this using the path /demos/rest/api via these methods:
        
        <p>
        <h3> GET </h3><br>
        
        curl -X GET \"http://nitrogenproject.com/demos/rest/api/name=m3rl1n&city=london\"

        <p>
        <h3> POST </h3><br>

        curl -X POST \"http://nitrogenproject.com/demos/rest/api\" -d '{\"name\": \"th31nitiate\", \"city\": \"London\"}'

        <p>
        You can send similar posts request using your favorite HTTP clients such as Postman. 
        "
    ].

event(_) -> ok.


