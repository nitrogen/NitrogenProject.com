%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_api_test).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

render_element(Record = #api_test{}) ->
	Callback = Record#api_test.callback,
    wf:wire(#api{ anchor=page, name=api_test_element, tag={callback, Callback}, delegate=?MODULE }),
	#button{
        text="An api button",
		id=Record#api_test.id,
		class=Record#api_test.class,
		click="page.api_test_element()"
	}.

api_event(api_test_element, {callback, Callback}, []) ->
	Callback().
