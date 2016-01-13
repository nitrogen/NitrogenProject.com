%% vim: ts=4 sw=4 et
-module(tests_dropdown).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
    wf_test:start(fun tests/0),
    #template{file="templates/grid.html"}.

layout() ->
    [
        test1_dropdown(),
        test2_dropdown(),
        test3_dropdown(),
        test4_dropdown(),
        test5_dropdown(),
        test6_dropdown(),
        test7_dropdown(),
        test8_dropdown()
    ].

test1_dropdown() ->
    %apple should be selected, by default
    #dropdown{id=test1, options=[
        #option{value=apple, text="apple"},
        #option{value=orange, text="orange"},
        #option{value=banana, text="banana"}
    ]}.

test2_dropdown() ->
    % banana should be selected
    #dropdown{id=test2, options=[
        #option{value=apple, text="apple"},
        #option{value=orange, text="orange"},
        #option{value=banana, text="banana", selected=true}
    ]}.

test3_dropdown() ->
    %% orange should be selected
    #dropdown{id=test3, value=orange, options=[
        #option{value=apple, text="apple"},
        #option{value=orange, text="orange"},
        #option{value=banana, text="banana"}
    ]}.

test4_dropdown() ->
    %% orange  or banana should should be selected. Browser behavior when two
    %% elements are selected is officially 'undefined'
    #dropdown{id=test4, value=orange, options=[
        #option{value=apple, text="apple"},
        #option{value=orange, text="orange"},
        #option{value=banana, text="banana", selected=true}
    ]}.

test5_dropdown() ->
    %% banana should still be selected because the orange option is explicitly set to false
    #dropdown{id=test5, value=orange, options=[
        #option{value=apple, text="apple"},
        #option{value=orange, text="orange", selected=false},
        #option{value=banana, text="banana", selected=true}
    ]}.

test6_dropdown() ->
    %% banana should still be selected because the orange option is explicitly set to false
    #dropdown{id=test6, value=banana, options=[
        {apple, "apple"},
        {orange, "orange"},
        {banana, "banana"}
    ]}.

test7_dropdown() ->
    %% apple should be selected (added from report from https://github.com/nitrogen/nitrogen_core/pull/98#issuecomment-171199670 )
    #dropdown{id=test7, value=apple, options=[
        #option{value=banana, text="Banana"},
        #option{value=apple, text="Apple"},
        #option{value=orange, text="Orange"}
   ]}.

test8_dropdown() ->
    %% orange should be selected
    #dropdown{id=test8, value = <<"orange">>, options=[
        #option{value="apple", text="apple"},
        #option{value="orange", text="orange"},
        #option{value="banana", text="banana"}
    ]}.

tests() ->
    ?wf_test_auto(test1_none_selected, undefined,               fun() -> wf:q(test1)=="apple" end),
    ?wf_test_auto(test2_last_selected, undefined,               fun() -> wf:q(test2)=="banana" end),
    ?wf_test_auto(test3_value_selected, undefined,              fun() -> wf:q(test3)=="orange" end),
    ?wf_test_auto(test4_value_selected_with_other, undefined,   fun() -> lists:member(wf:q(test4), ["orange","banana"]) end),
    ?wf_test_auto(test5_value_selected_with_false, undefined,   fun() -> wf:q(test5)=="banana" end),
    ?wf_test_auto(test6_proplist_options, undefined,            fun() -> wf:q(test6)=="banana" end),
    ?wf_test_auto(test7_shuffled, undefined,                    fun() -> wf:q(test7)=="apple" end),
    ?wf_test_auto(test8_mixed_types, undefined,                 fun() -> wf:q(test8)=="orange" end).
