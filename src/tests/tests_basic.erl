-module(tests_basic).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	wf_test:start(fun tests/0),
 	#template{file="priv/templates/grid.html"}.

layout() ->
	#panel{id=wrapper}.

event(slow_postback) ->
    ?PRINT(received_hit),
    wf:send(slow_postback_tracker, hit),
    timer:sleep(12000),
    wf:send(slow_postback_tracker, finished),
    wf:insert_bottom(wrapper, #panel{text="Postback Complete"});
event(Tag) ->
	?wf_test_event(Tag).

tests() ->
	?wf_test_manual(default_postback, fun default_postback/0, fun verify_default_postback/0),
	?wf_test_manual(basic_postback, fun basic_postback/0, fun verify_basic_postback/0),
	?wf_test_manual(click_postback, fun click_postback/0, fun verify_click_postback/0),
	?wf_test_manual(delayed_postback_with_session, fun delayed_postback/0, fun verify_delayed_postback/0),
    ?wf_test_manual(slow_postback, fun slow_postback/0, fun verify_slow_postback/0, [{timeout, 25000}]).


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
	wf:session(start, os:timestamp()),
	wf:wire(#event{type=timer, delay=1000, postback=delayed_postback_with_session}).


verify_delayed_postback() ->
	Then = milliseconds(wf:session(start)),
	Now = milliseconds(os:timestamp()),
	Diff = Now - Then,
	Diff >= 1000. 

milliseconds({MS, S, US}) -> 
	(MS*1000000000) + (S*1000) + (US div 1000).

slow_postback() ->
    wf:session(slow_postback_hits, 0),
    wf:insert_bottom(wrapper, #panel{text="Initiating slow postback 10 seconds"}),
    wf:comet(fun init_slow_postback_tracker/0, slow_postback_tracker),
    wf:defer(#event{postback=slow_postback}).

init_slow_postback_tracker() ->
    wf:wire("Nitrogen.$last_websocket_received=null"), %% This just forces some key changes to force a ping test
    wf:flush(),
    slow_postback_tracker().

slow_postback_tracker() ->
    receive
        hit ->
            OrigHits = wf:session(slow_postback_hits),
            NewHits = OrigHits + 1,
            wf:session(slow_postback_hits, NewHits),
            %wf:wire(#alert{text="t3est"}),
            %wf:insert_bottom(wrapper, #panel{text="Posted a hit"}),
            wf:flush(),
            slow_postback_tracker();
        finished ->
            wf:insert_bottom(wrapper, #panel{text="Comet tracker finished. Will wait a few seconds before moving on"}),
            wf:flush(),
            ?wf_test_event(slow_postback)
    end.

verify_slow_postback() ->
    Hits = wf:session(slow_postback_hits),
    Hits == 1.
