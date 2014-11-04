-module(tests_advancedcontrols2).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	wf_test:start_other(demos_advancedcontrols2, fun tests/0).

tests() ->
	timer:sleep(1000), %% give google charts a chance to respond
	?wf_test_js(line_chart, chart_test(line_chart)),
	?wf_test_js(bar_chart, chart_test(bar_chart)),
	?wf_test_js(pie_chart, chart_test(pie_chart)).

chart_test(ID) ->
	{
		undefined,
		wf:f("return objs('~s').width()", [ID]),
		fun([Val]) -> Val==400 end
	}.
