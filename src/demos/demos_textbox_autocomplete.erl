-module (demos_textbox_autocomplete).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Autocompletion".

headline() -> "Autocompletion".

left() -> 
    [
        "
        <p>
		The <code>#textbox_autocomplete{}</code> element gives you a simple way
		to add autocompletion with options to a textbox element. It relies on
		JSON encoding, and expects your application to have two functions
		exported: <code>autocomplete_enter_event(SearchTerm, Tag)</code>, which
		is called as the user types and
		<code>autocomplete_select_event(SelectedElement, Tag)</code>, which is
		called when the user selects and element from the list.
        ",
        linecount:render()
    ].

right() -> 
    [
        #p{},
        #label { text="What's your favorite programming language?" },
        #textbox_autocomplete { tag=auto1, minLength=1 },
        #flash {}
    ].

does_search_match(LangRec, SearchTerm0) ->
	%% We extract the label from the passed Lang "Rec" (which is just a proplist)
	Label0 = proplists:get_value(label, LangRec),
	%% And convert it ot lower case
	Label = string:to_lower(wf:to_unicode_list(Label0)),
	%% and also convert the SearchTerm to lowercase
	SearchTerm = string:to_lower(SearchTerm0),
	%% Then see if SearchTerm is contained anywhere in the Label
    string:str(Label, SearchTerm) > 0.

autocomplete_enter_event(SearchTerm, _Tag) ->
	Data0 = base_data(),
	
	%% Let's filter the list based on the SearchTerm
    Data = [LangRec || LangRec <- Data0, does_search_match(LangRec, SearchTerm)],

	%% Encode the Data into json for the autocomplete event. As you can see
	%% from the base_data function below, it expects each record to have an
	%% "id", a "label", and a "value". "id" is a short-hand identifier. "label"
	%% is what will be displayed in the dropdown, and "value" will be passed
	%% along with the "id", then decoded from json and sent to the
	%% autocomplete_select_event function as a proplist.
    wf:json_encode(Data).

autocomplete_select_event(SelectedElement, _Tag) ->
	%% SelectedElement is a proplist
	%% Let's tell the user what we selected!
    wf:flash(proplists:get_value(<<"value">>, SelectedElement)),
    ok.

base_data() ->
	[
		[{id, <<"c">>}, {label, <<"C">>}, {value, <<"C">>}],
		[{id, <<"cpp">>}, {label, <<"C++">>}, {value, <<"C++">>}],
		[{id, <<"clojure">>}, {label, <<"Clojure">>}, {value, <<"Clojure">>}],
		[{id, <<"coffeescript">>}, {label, <<"Coffeescript">>} , {value, <<"Coffeescript">> }],
		[{id, <<"elixir">>}, {label, <<"Elixir">>} , {value, <<"Elixir">> }],
		[{id, <<"erlang">>}, {label, <<"Erlang">>} , {value, <<"Erlang">> }],
		[{id, <<"go">>}, {label, <<"Golang">>} , {value, <<"Golang">> }],
		[{id, <<"haskell">>}, {label, <<"Haskell">>} , {value, <<"Haskell">> }],
		[{id, <<"lua">>}, {label, <<"Lua">>} , {value, <<"Lua">> }],
		[{id, <<"java">>}, {label, <<"Java">>} , {value, <<"Java">> }],
		[{id, <<"js">>}, {label, <<"Javascript">>} , {value, <<"Javascript">> }],
		[{id, <<"perl">>}, {label, <<"Perl">>} , {value, <<"Perl">> }],
		[{id, <<"php">>}, {label, <<"PHP">>} , {value, <<"PHP">> }],
		[{id, <<"python">>}, {label, <<"Python">>}, {value, <<"Python">>}],
		[{id, <<"objc">>}, {label, <<"Objective-C">>} , {value, <<"Objective-C">> }],
		[{id, <<"ocaml">>}, {label, <<"OCaml">>} , {value, <<"OCaml">> }],
		[{id, <<"ruby">>}, {label, <<"Ruby">>} , {value, <<"Ruby">> }],
		[{id, <<"scala">>}, {label, <<"Scala">>} , {value, <<"Scala">> }],
		[{id, <<"swift">>}, {label, <<"Swift">>} , {value, <<"Swift">> }]
	].

