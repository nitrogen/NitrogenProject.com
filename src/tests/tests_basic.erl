-module(tests_basic).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	wf_test:start(fun tests/0),
 	#template{file="templates/grid.html"}.

layout() ->
	#panel{id=wrapper}.

event(Tag) ->
	?wf_test_event(Tag).

tests() ->
	?wf_test_manual(default_postback, fun default_postback/0, fun verify_default_postback/0),
	?wf_test_manual(basic_postback, fun basic_postback/0, fun verify_basic_postback/0),
	?wf_test_manual(click_postback, fun click_postback/0, fun verify_click_postback/0),
	?wf_test_manual(delayed_postback_with_session, fun delayed_postback/0, fun verify_delayed_postback/0).


default_postback() ->
	wf:update(wrapper, #textbox{id=textbox, text="Default"}),
	wf:wire(#event{postback=default_postback}).

verify_default_postback() ->
	wf:q(textbox) == "Default".

basic_postback() ->
	wf:set(textbox, "New"),
	wf:wire(#event{postback=basic_postback}).

verify_basic_postback() ->
	wf:q(textbox) == "New".

click_postback() ->
	wf:insert_bottom(wrapper, #button{id=click_postback, postback=click_postback, text="clicker"}),
	wf:defer(click_postback, #click{}).

verify_click_postback() ->
	wf:q(textbox) == "New".

delayed_postback() ->
	wf:session(start, now()),
	wf:wire(#event{type=timer, delay=1000, postback=delayed_postback_with_session}).

verify_delayed_postback() ->
	Then = milliseconds(wf:session(start)),
	Now = milliseconds(now()),
	Diff = Now - Then,
	Diff >= 1000.

milliseconds({MS, S, US}) ->
	(MS*1000000000) + (S*1000) + (US div 1000).
