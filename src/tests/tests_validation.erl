-module(tests_validation).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() ->
	wf_test:start_other(demos_validation, fun tests/0).

tests() ->
	%% suppress any alert boxes for this page
	wf:wire("alert = function() { };"),
	reset_values(),
	?wf_test_js(blank_test, blank_test()),
	?wf_test_js(fullname_test, fullname_test()),
	?wf_test_js(allpass_test, allpass_test()),
	?wf_test_js(passwordconfirm_test, passwordconfirm_test()),
	?wf_test_js(shortpassword_test, shortpassword_test()),
	?wf_test_js(badnumber_test, badnumber_test()),
	?wf_test_js(number_out_of_range_test, number_out_of_range_test()),
	?wf_test_js(unicode_value_test, unicode_value_test()),
	?wf_test_js(unicode_comparison_test, unicode_comparison_test()),
	?wf_test_js(unicode_comparison_pass_test, unicode_comparison_pass_test()),
	?wf_test_js(unicode_validator_test, unicode_validator_test()).


-define(click, wf:wire(continueButton, #click{})).
-define(count, "return $('.LV_validation_message').length;").
-define(set(FV), fun() -> set(FV), ?click end).
-define(opts, [{delay, 300}]).

blank_test() ->
	{
		?set([]),
		?count,
		fun([Length]) -> Length==5 end,
		?opts
	}.

fullname_test() ->
	{
		?set([
			{nameTextBox, "jesse"}, %% wants "rusty"
			{emailTextBox, "email@email.com"},
			{passwordTextBox, "password"},
			{confirmTextBox, "password"},
			{numberTextBox, "1"}
		]),
		?count,
		fun([Length]) -> Length == 1 end,
		?opts
	}.

allpass_test() ->
	{
		?set([
			{nameTextBox, "rusty lastname"},
			{emailTextBox, "email@email.com"},
			{passwordTextBox, "password"},
			{confirmTextBox, "password"},
			{numberTextBox, "1"}
		]),
		?count,
		fun([Length]) -> Length == 0 end,
		?opts
	}.

passwordconfirm_test() ->
	{
		?set([
			{nameTextBox, "rusty lastname"},
			{emailTextBox, "email@email.com"},
			{passwordTextBox, "password"},
			{confirmTextBox, "passwordother"}, %% password doesn't match
			{numberTextBox, "1"}
		]),
		?count,
		fun([Length]) -> Length == 1 end,
		?opts
	}.

	
shortpassword_test() ->
	{
		?set([
			{nameTextBox, "rusty lastname"},
			{emailTextBox, "email@email.com"},
			{passwordTextBox, "pass"}, %%password should be at least 6 chars
			{confirmTextBox, "pass"},
			{numberTextBox, "1"}
		]),
		?count,
		fun([Length]) -> Length == 1 end,
		?opts
	}.

badnumber_test() ->
	{
		?set([
			{nameTextBox, "rusty lastname"},
			{emailTextBox, "email@email.com"},
			{passwordTextBox, "password"},
			{confirmTextBox, "password"},
			{numberTextBox, "NaN"} %% not a number
		]),
		?count,
		fun([Length]) -> Length == 1 end,
		?opts
	}.

number_out_of_range_test() ->
	{
		?set([
			{nameTextBox, "rusty lastname"},
			{emailTextBox, "email@email.com"},
			{passwordTextBox, "password"},
			{confirmTextBox, "password"},
			{numberTextBox, "11"} %% range should be 1-10
		]),
		?count,
		fun([Length]) -> Length == 1 end,
		?opts
	}.


unicode_value_test() ->
	{
		?set([
			{nameTextBox, "Тест ыыы"},
			{emailTextBox, "email@email.com"},
			{passwordTextBox, "password"},
			{confirmTextBox, "password"},
			{numberTextBox, "10"} %% range should be 1-10
		]),
		?count,
		fun([Length]) -> Length == 1 end,
		?opts
	}.

unicode_comparison_test() ->
	{
		fun() ->
			wf:wire(continueButton, otherTextBox, #validate{validators=[
				#custom{
					text="Must start with 'Тест'",
					tag=whatever,
					function=fun(_, V) ->
								case V of 
									"Тест" ++ _ -> true;
									_ -> false
								end
							 end
				}
			]}),
			?set([
				{otherTextBox, "not-valid"},
				{nameTextBox, "rusty lastname"},
				{emailTextBox, "email@email.com"},
				{passwordTextBox, "password"},
				{confirmTextBox, "password"},
				{numberTextBox, "10"} %% range should be 1-10
			])()
		end,
		?count,
		fun([Length]) -> Length == 1 end,
		?opts
	}.

unicode_comparison_pass_test() ->
	{
		?set([
			{otherTextBox, "Тест blahblah"},
			{nameTextBox, "rusty klophaus"},
			{emailTextBox, "email@email.com"},
			{passwordTextBox, "password"},
			{confirmTextBox, "password"},
			{numberTextBox, "10"} %% range should be 1-10
		]),
		?count,
		fun([Length]) -> Length == 0 end,
		?opts
	}.

unicode_validator_test() ->
	{
		fun() ->
			wf:eager(continueButton, otherTextBox, #validate{validators=#is_required{text="Тест ыыы"}}),
			?set([
				{otherTextBox, ""},
				{nameTextBox, "rusty lastname"},
				{emailTextBox, "email@email.com"},
				{passwordTextBox, "password"},
				{confirmTextBox, "password"},
				{numberTextBox, "10"} %% range should be 1-10
			])()
		end,
		?count,
		fun([Length]) -> Length == 1 end,
		?opts
	}.



set(FieldValues) ->	
	wf:wire("$('.LV_validation_message').remove();"),
	[wf:set(F, V) || {F,V} <- FieldValues].

reset_values() ->
	Fields = [nameTextBox, emailTextBox, passwordTextBox,
			  confirmTextBox, otherTextBox, numberTextBox],
	[wf:set(F, "") || F <- Fields].

