%% vim: ts=4 sw=4 et
-module(tests_api).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() ->
    wf_test:start(fun tests/0),
    #template{file="priv/templates/grid.html"}.

layout() ->
    wf:wire(#api{name=test_api}),
    #panel{id=updates}.


tests() ->
    ?wf_test_manual(one_arg, one_arg()),
    ?wf_test_manual(two_arg, two_arg()),
    ?wf_test_manual(two_byte_string, two_byte_string()),
    ?wf_test_manual(four_byte_string, four_byte_string()),
    ?wf_test_manual(really_long_string, really_long_string()),
    ?wf_test_manual(api_within_element, api_within_element()).

one_arg() ->
    String = random_string(16),
    one_arg(one_arg, String).

two_byte_string() ->
    String = random_string(65535),
    one_arg(two_byte_string, String).

four_byte_string() ->
    String = random_string(65536),
    one_arg(four_byte_string, String).

really_long_string() ->
    String = random_string(1024*1024),
    one_arg(really_long_string, String).

one_arg(TestName, String) ->
    wf:session(api_test_name, TestName),
    {
        fun() ->
            wf:session(arg1, ""),
            wf:insert_bottom(updates, wf:f("Testing ~s<br>", [TestName])),
            wf:wire(wf:f("page.test_api(\"~s\")",[String]))
        end,
        fun() ->
            Arg1 = wf:session(arg1),
            Arg1 = String,
            true
        end,
        [{timeout, 10000}] %% 10 seconds because "reall_long_string" takes a while to encode
    }.

two_arg() ->
    String1 = random_string(16),
    String2 = random_string(1000),
    {
        fun() ->
            wf:session(arg1, ""),
            wf:session(arg2, ""),
            wf:insert_bottom(updates, "Testing two_arg<br>"),
            wf:wire(wf:f("page.test_api(\"~s\", \"~s\")",[String1, String2]))
        end,
        fun() ->
            String1 = wf:session(arg1),
            String2 = wf:session(arg2),
            true
        end
    }.

api_within_element() ->
    {
        fun() ->
            wf:insert_bottom(updates, "Testing api_within_element<br>"),
            wf:insert_after(updates, #api_test{id=api_button, callback=fun test_element_complete/0}),
            wf:wire(api_button, #event{type=timer, delay=500, actions=#click{}})
        end,
        fun() ->
            true
        end
    }.

test_element_complete() ->
    wf_test:event(api_within_element).

api_event(test_api, _, [Arg1]) ->
    wf:session(arg1, Arg1), 
    TestName = wf:session(api_test_name),
    wf_test:event(TestName);
api_event(test_api, _, [Arg1, Arg2]) ->
    wf:session(arg1, Arg1),
    wf:session(arg2, Arg2),
    wf_test:event(two_arg).

random_string(Length) ->
    [?WF_RAND_UNIFORM($a, $z+1) || _ <- lists:seq(1, Length)].
