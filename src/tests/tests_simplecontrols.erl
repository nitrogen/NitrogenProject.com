-module(tests_simplecontrols).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() ->
	wf_test:start_other(demos_simplecontrols, fun tests/0).

tests() ->
    ?wf_test_auto(textbox, test_textbox("Test Value")),
    ?wf_test_auto(blank_textbox, test_textbox("")),
    ?wf_test_auto(textarea, test_textarea()),
    ?wf_test_auto(dropdown, test_dropdown()),
    ?wf_test_auto(blank_dropdown, test_blank_dropdown()),
    ?wf_test_auto(radio, undefined, fun check_radio/0),
    ?wf_test_auto(checkbox, test_checkbox()),
    ?wf_test_auto(checkbox_changed, test_checkbox_changed()),
    ?wf_test_auto(insert_after_order, test_insert_after_order()),
    ?wf_test_auto(blank_multi_dropdown, test_multidropdownblank()),
    ?wf_test_auto(multi_dropdown, test_multidropdown()),
    ?wf_test_auto(multi_dropdown1, test_multidropdown1()),
    ?wf_test_manual(click_postback, test_remove_multi_click_postback()).

test_textbox(Str) ->
    {
        fun() -> wf:set(textbox, Str) end,
        fun() -> wf:q(textbox) == Str end
    }.

test_textarea() ->
    Str = "A long-form\nString",
    {
        fun() -> wf:set(textarea, Str) end,
        fun() -> wf:q(textarea)==Str end
    }.

test_dropdown() ->
    {
        fun() -> wf:set(my_dropdown, "opt2") end,
        fun() -> wf:q(my_dropdown) == "opt2" end
    }.

test_blank_dropdown() ->
	{
		fun() -> wf:set(my_dropdown, "invalid") end,
		fun() -> wf:q(my_dropdown) == "" end
	}.

test_checkbox() ->
    {
        undefined,
        fun() -> wf:qs(checkbox) == ["check1","check3"] end
    }.

test_checkbox_changed() ->
    {
        fun() -> wf:set_multiple(checkbox, ["check2", "check3"]) end,
        fun() -> wf:qs(checkbox) == ["check2","check3"] end
    }.

test_insert_after_order() ->
    {
        fun() ->
            wf:insert_after(disabled_button, #textbox{id=order1, text="1"}),
            wf:insert_after(disabled_button, #textbox{id=order1, text="2"}),
            wf:insert_after(disabled_button, #textbox{id=order1, text="3"})
        end,
        fun() ->
            wf:qs(order1) == ["3","2","1"]
        end
    }.

test_multidropdownblank() ->
    {
        fun() -> wf:set_multiple(multiple, []) end,
        fun() -> wf:qs(multiple) == [] end
    }.

test_multidropdown() ->
    {
        fun() -> wf:set_multiple(multiple, ["1", "2", "3"]) end,
        fun() -> wf:qs(multiple) == ["1","2","3"] end
    }.

test_multidropdown1() ->
    {
        fun() -> wf:set_multiple(multiple, ["2"]) end,
        fun() -> wf:qs(multiple) == ["2"] end
    }.

check_radio() ->
    wf:q(myRadio)=="1".

test_remove_multi_click_postback() ->
    {
        fun() ->
            wf:replace(disabled_button, #button{text="Test Button", id=my_new_button, delegate=wf_test, postback=click_postback}),
            wf:remove(multiple),
            wf:defer(my_new_button, #click{})
        end,
        fun() ->
            wf:qs(multiple)==[]
        end
    }.
