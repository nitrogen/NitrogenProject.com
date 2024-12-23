-module(tests_advancedcontrols1).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() ->
	wf_test:start_other(demos_advancedcontrols1, fun tests/0).

tests() ->
	?wf_test_js(inplace_basic, inplace_check_basic()),
	?wf_test_js(inplace_cancel, inplace_check_cancel()),
	?wf_test_js(inplace_validation, inplace_check_validation()),
	?wf_test_js(inplace_textarea, inplace_check_textarea()),
	?wf_test_js(inplace_dropdown1, inplace_check_dropdown1()),
	?wf_test_js(inplace_dropdown2, inplace_check_dropdown2()),
	?wf_test_js(inplace_dropdown3, inplace_check_dropdown3()),
    ?wf_test_js(inplace_complex1, inplace_complex1()).

-define(DELAY, [{delay, 500}]).

%% A side effect here with the postback request is that the postback's
%% page_module is calculated as tests_advancedcontrols1 (due to the path and
%% the page_module not being transferred with the postback context.
inplace_textbox_event(_Tag, Val) ->
	Val.

inplace_textarea_event(_Tag, Val) ->
	Val.

inplace_event(_Tag, Val) ->
    Val.

inplace_check_basic() ->
	{
        fun() ->    
            %% set textbox1 to "New Value"
            wf:wire("##textbox1 > .view", #click{}),
            wf:set("##textbox1 > .edit > input[type=text]", "New Value"),
            wf:wire("##textbox1 > .edit > .inplace_ok", #click{})
        end,
		"return $('.wfid_textbox1 > .view > .label').text()",
		fun([Val]) -> Val == "New Value" end,
        ?DELAY
	}.

inplace_check_cancel() ->
	{
        fun() ->
            wf:insert_after(textbox1, #inplace_textbox{id=textbox_cancel, text="Original", tag=anything}),

            %% Change value of textbox_cancel to "Cancel Value", then cancel
            wf:wire("##textbox_cancel > .view", #click{}),
            wf:set("##textbox_cancel > .edit > input[type=text]", "Cancel Value"),
            wf:wire("##textbox_cancel > .edit > .inplace_cancel", #click{})
        end,
		"return $('.wfid_textbox_cancel > .view > .label').text()",
		fun([Val]) -> Val == "Original" end,
        ?DELAY
	}.

inplace_check_validation() ->
    {
        fun() ->
            %% Change value of textbox2 to "Cancel Value", then cancel
            wf:wire("##textbox2 > .view", #click{}),
            wf:set("##textbox2 > .edit > input[type=text]", "123Not a Number"),
            wf:wire("##textbox2 > .edit > .inplace_ok", #click{})
        end,
        "return $('.wfid_textbox2 > .edit > input[type=text]').hasClass(Nitrogen.$validation_failed_class)",
        fun([TF]) -> TF end,
        ?DELAY
    }.


inplace_check_textarea() ->
	{
        fun() ->
            %% Change value of textarea to "Other Value"
            wf:wire("##textarea > .view", #click{}),
            wf:set("##textarea > .edit > textarea", "Other Value"),
            wf:wire("##textarea > .edit > .inplace_ok", #click{})
        end,
		"return $('.wfid_textarea > .view > .label').text()",
		fun([Val]) -> Val == "Other Value" end,
        ?DELAY
	}.

inplace_check_dropdown1() ->
    inplace_check_dropdown(dropdown1, "Option 3").

inplace_check_dropdown2() ->
    inplace_check_dropdown(dropdown2, "Option 2").

inplace_check_dropdown3() ->
    inplace_check_dropdown(dropdown3, "Option 4").

inplace_check_dropdown(ID, NewVal) ->
    IDStr = "##" ++ wf:to_list(ID),
    IDClass = ".wfid_" ++ wf:to_list(ID),
	{
        fun() ->
            %% Change value of textarea to "Other Value"
            wf:wire(IDStr ++ " > .view", #click{}),
            wf:set(IDStr ++ " > .edit select", NewVal),
            wf:wire(IDStr ++ " > .edit .inplace_ok", #click{})
        end,
		"return $('" ++ IDClass ++ " > .view .label').text()",
		fun([Val]) -> Val == NewVal end,
        ?DELAY
	}.

inplace_complex1() ->
    Opts = demos_advancedcontrols1:hex_options(),
    Items = length(Opts),
    R = lists:nth(rand:uniform(Items), Opts),
    G = lists:nth(rand:uniform(Items), Opts),
    B = lists:nth(rand:uniform(Items), Opts),
    ColorCode = "#" ++ R ++ G ++ B,

    ID = "color", 
    IDStr = "##" ++ wf:to_list(ID),
    IDClass = ".wfid_" ++ wf:to_list(ID),
	{
        fun() ->
            %% Change value of textarea to "Other Value"
            wf:wire(IDStr ++ " > .view", #click{}),
            wf:set(IDStr ++ " > .edit .red_dd", R),
            wf:set(IDStr ++ " > .edit .green_dd", G),
            wf:set(IDStr ++ " > .edit .blue_dd", B),
            wf:wire(IDStr ++ "> .edit .blue_dd", #click{}),

            wf:wire(IDStr ++ " > .edit .inplace_ok", #click{})
        end,
		"return $('" ++ IDClass ++ " > .view .label').text()",
		fun([Val]) -> Val == ColorCode end,
        ?DELAY
	}.


