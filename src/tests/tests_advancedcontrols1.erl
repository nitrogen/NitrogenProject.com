-module(tests_advancedcontrols1).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() ->
	wf_test:start_other(demos_advancedcontrols1, fun tests/0).

tests() ->
	?wf_test_auto(inplace_setup, fun inplace_setup/0, undefined),
	?wf_test_js(inplace_check1, inplace_check1()),
	?wf_test_js(inplace_check2, inplace_check2()),
	?wf_test_js(inplace_check3, inplace_check3()).

%% A side effect here with the postback request is that the postback's
%% page_module is calculated as tests_advancedcontrols1 (due to the path and
%% the page_module not being transferred with the postback context.
inplace_textbox_event(_, Val) ->
	Val.

inplace_textarea_event(_, Val) ->
	Val.

%% This initial setup just performs a handful of changes, but with the current
%% version of the nitrogen test suite, we need to set up first, then when the
%% test completes (that is, the postbacks finish completing it all).
inplace_setup() ->
	%% set textbox1 to "New Value"
	wf:wire("##textbox1 > .view", #click{}),
	wf:set("##textbox1 > .edit > .textbox", "New Value"),
	wf:wire("##textbox1 > .edit > .inplace_ok", #click{}),

	%% Change value of textbox2 to "Cancel Value", then cancel
	wf:wire("##textbox2 > .view", #click{}),
	wf:set("##textbox2 > .edit > .textbox", "Cancel Value"),
	wf:wire("##textbox2 > .edit > .inplace_cancel", #click{}),

	%% Change value of textarea to "Other Value"
	wf:wire("##textarea > .view", #click{}),
	wf:set("##textarea > .edit > .textarea", "Other Value"),
	wf:wire("##textarea > .edit > .inplace_ok", #click{}).


inplace_check1() ->
	{
		undefined,
		"return $('.wfid_textbox1 > .view > .label').text()",
		fun([Val]) -> Val == "New Value" end
	}.

inplace_check2() ->
	{
		undefined,
		"return $('.wfid_textbox2 > .view > .label').text()",
		fun([Val]) -> Val == "Sample Text 2." end
	}.

inplace_check3() ->
	{
		undefined,
		"return $('.wfid_textarea > .view > .inplace_textarea').text()",
		fun([Val]) -> Val == "Other Value" end
	}.
