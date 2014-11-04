-module(tests_disable_encoding).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() ->
	wf_test:start_other(demos_contenttype, fun tests/0).

tests() ->
	?wf_test_js(verify_spaceman_width, verify_spaceman("width", 32)),
	?wf_test_js(verify_spaceman_height, verify_spaceman("height", 50)).

verify_spaceman(Metric, Expected) ->
	{
		undefined,
		"return objs('spaceman')." ++ Metric ++ "()",
		fun([Val]) -> Val==Expected end,
		[{delay, 100}]
	}.
	
