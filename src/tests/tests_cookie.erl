-module(tests_cookie).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() ->
	case wf:q("step") of
		"1" -> step1_main();
		"2" -> step2_main();
		"3" -> step3_main()
	end,
	#template{file="templates/grid.html"}.

step1_main() ->
	InitialCookieVal = wf_utils:guid(),
	wf:session(test_cookie_val, InitialCookieVal),
	wf:cookie(test_cookie_val, InitialCookieVal),
	wf_test:start(fun step1_tests/0).

step2_main() ->
	wf_test:start(fun step2_tests/0).

step3_main() ->
	wf_test:start(fun step3_tests/0).

step1_tests() ->
	?wf_test_auto(get_cookie, get_cookie_test()),
	?wf_test_auto(set_cookie, set_cookie_test()).

get_cookie_test() ->
	{
		undefined,
		fun verify_cookie_fun/0
	}.

set_cookie_test() ->
	{
		fun() ->
			NewVal = wf_utils:guid(),
			wf:cookie(test_cookie_val, NewVal),
			wf:session(test_cookie_val, NewVal)
		end,
		undefined
	}.


step2_tests() ->
	%% Makes sure cookie value is retained across multiple requests
	?wf_test_auto(get_cookie, get_cookie_test()),
	?wf_test_auto(delete_cookie, delete_cookie_test()).

step3_tests() ->
	?wf_test_auto(check_deleted_cookie, check_deleted_cookie()).

delete_cookie_test() ->
	{
		fun() -> wf:delete_cookie(test_cookie_val) end,
		undefined
	}.

check_deleted_cookie() ->
	{
		undefined,
		fun() -> wf:cookie(test_cookie_val) == undefined end
	}.
	
verify_cookie_fun() ->
	ExpectedCookie = wf:session(test_cookie_val),
	ActualCookie = wf:cookie(test_cookie_val),
	case ExpectedCookie == ActualCookie of
		true -> true;
		false -> false
	end.
