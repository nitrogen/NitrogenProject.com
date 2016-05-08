-module(tests_cookie).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() ->
	case wf:q("step") of
		"1" -> step1_main();
		"2" -> step2_main();
		"3" -> step3_main();
        "4" -> step4_main()
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

step4_main() ->
    InitialCookieVal = wf_utils:guid(),
    OtherCookieVal = wf_utils:guid(),
    wf:session(test_cookie_val, InitialCookieVal),
    wf:cookie(test_cookie_val, InitialCookieVal, [{http_only, true}]),
    wf:cookie(other_test_cookie_val, OtherCookieVal),
    wf_test:start(fun() -> step4_tests(OtherCookieVal) end).

step1_tests() ->
	?wf_test_auto(get_cookie, get_cookie_test()),
	?wf_test_auto(set_cookie, set_cookie_test([])).

get_cookie_test() ->
	{
		undefined,
		fun verify_cookie_fun/0
 	}.

get_cookie_js_test(Cookie, ExpectedVal) ->
    {
        undefined,
        "   cname='" ++ wf:to_list(Cookie) ++ "';
            var name = cname + '=';
            var ca = document.cookie.split(';');
            for(var i = 0; i <ca.length; i++) {
                var c = ca[i];
                while (c.charAt(0)==' ') {
                    c = c.substring(1);
                }
                if (c.indexOf(name) == 0) {
                    return c.substring(name.length,c.length);
                }
            }
            return '';",
        fun([ActualVal]) ->
            ActualVal==ExpectedVal
        end
    }.

set_cookie_test(Opts) ->
	{
		fun() ->
			NewVal = wf_utils:guid(),
			wf:cookie(test_cookie_val, NewVal, Opts),
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

step4_tests(OtherVal) ->
    ?wf_test_auto(http_only_get_cookie, get_cookie_test()),
    ?wf_test_js(non_http_only_get_cookie_js, get_cookie_js_test(other_test_cookie_val, OtherVal)),
    ?wf_test_js(http_only_get_cookie_js, get_cookie_js_test(test_cookie_val, "")).
    %% FIXME: http_only cookies cannot be set in javascript. So wf:cookie
    %% called in a websocket postback *must* generate a separate HTTP request
    %% to do so. For now, we'll comment this out.
    %?wf_test_auto(http_only_set_cookie_in_postback, set_cookie_test([{http_only, true}])),

    %% NOTE: We have to comment this next test out because of a shortcoming
    %% with setting cookies in a websocket postback. And that is that cookies set
    %% in a postback don't change the cookies for the existing websocket
    %% connection.  Meaning that, when a postback sets a cookie and in a later
    %% postback attemps wf:cookie/1, the value will still be the non-current
    %% cookie.
    %%
    %% A potential fix for this would be to add something like
    %% wf:defer("Nitrogen.$restart_websocket()") to the #set_cookie{} action,
    %% thereby ensuring that if a cookie is set, a new websocket connection is
    %% established with the new cookie values.
    %?wf_test_auto(http_only_get_cookie_in_postback, get_cookie_test()).

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
