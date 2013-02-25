-module (demos_textboxlist).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "TextboxList".

headline() -> "Textboxlist".

left() -> 
    [
        "
        <p>
        The <code>#textboxlist{}</code> element effectively turns the input to a list of items that can be deleted.
        </p>
        <p>
        The element configured to autocomplete the values.
        </p>
        ",
        linecount:render()
    ].

right() -> 
    [
        #p{},
        #label{ text="Enter tags" },
        #textboxlist{ id=first, tag=auto1, autocomplete=false },
        #button { id=submit2, text="Submit", postback= {submit, first} },
        #p{},
        #label { text="What's your favorite programming language?" },
        #textboxlist { id=second, tag=auto2, values=["Erlang"] },
        #button { id=submit1, text="Submit", postback= {submit, second} },
        #p{},
        #flash{}
    ].

textboxlist_event(SearchTerm, _Tag) ->
  Data = ["Perl", "Php", "Erlang", "Ruby", "Scala"],
  List = [{array, [
    list_to_binary(T), % Id
    list_to_binary(T), % Bit plain text
    list_to_binary(T), % Bit html
    list_to_binary(T)  % Suggestion item html
  ] }
  || T <- lists:filter(fun(E)-> string:str(string:to_lower(E), string:to_lower(SearchTerm)) > 0 end, Data)],
  mochijson2:encode(List).

event({submit, Id})->
  wf:flash(wf:qs(Id));
event(_) -> ok.
