-module(tests_unicode).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() ->
	wf_test:start(fun tests/0),
	#template{file="templates/grid.html"}.

which_string(StringNum) ->
	N = wf:to_integer(wf:path_info()),
	element(StringNum, lists:nth(N, strings())).
		

strings() ->
	%% Miscellaneous translations from google translate
	[
		{"We're going to need a bigger boat", "Yes sir, the check is in the mail."},
		{"نحن ذاهبون في حاجة إلى قارب أكبر", "نعم يا سيدي، والاختيار هو في البريد."},
		{"我们将需要更大的船","是的，先生，支票在邮寄。"},
		{"आम्ही एका मोठ्या बोट गरज आहोत.","सर होय, चेक मेल आहे."}, 
		{"Budeme potřebovat větší loď","Ano, pane, kontrola je v e-mailu."},
		{"Мы собираемся нуждаться в большей лодку","Да, сэр, проверка по почте."},
		{"ჩვენ ვაპირებთ, რომ უნდა დიდი ნავი","დიახ სერ, შემოწმება ფოსტა."}
	].


title() -> which_string(1).

layout() ->
	#panel{id=wrapper, body=which_string(1)}.

tests() ->
	?wf_test_js(title_test, title_test()),
	?wf_test_js(original_body_test, original_body_test()),
	?wf_test_js(test_encoded, test_encoded()),
	?wf_test_js(test_raw, test_raw()),
	?wf_test_auto(add_unicode_textbox, add_unicode_textbox()),
	?wf_test_auto(set_unicode_textbox_raw, set_unicode_textbox_raw()),
	?wf_test_auto(add_unicode_dropdown, add_unicode_dropdown()),
	?wf_test_auto(set_unicode_dropdown_raw, set_unicode_dropdown_raw()),
	ok.

title_test() ->
	{
		undefined,
		"return $('title').text();",
		fun([V]) -> V == "Nitrogen - " ++ which_string(1) end
	}.

original_body_test() ->
	{
		undefined,
		"return objs('wrapper').text();",
		fun([V]) -> V == which_string(1) end
	}.

test_encoded() ->
	Str = which_string(2),
	{
		fun() -> wf:update(wrapper, wf:html_encode(Str)) end,
		"return objs('wrapper').text();",
		fun([V]) -> V == Str end
	}.

test_raw() ->
	Str = which_string(2),
	{
		%% while test_encoded/0 encoded it with wf:html_encode, this just sends the raw unicode strings
		fun() -> wf:update(wrapper, Str) end,
		"return objs('wrapper').text();",
		fun([V]) -> V == Str end
	}.

add_unicode_textbox() ->
	Str = which_string(1),
	{
		fun() -> wf:insert_bottom(wrapper, #textbox{id=unicode, text=Str}) end,
		fun() -> wf:q(unicode)==Str end
	}.


set_unicode_textbox_raw() ->
	Str = which_string(2),
	{
		fun() -> wf:set(unicode, Str) end,
		fun() -> wf:q(unicode)==Str end
	}.

add_unicode_dropdown() ->
    Str1 = which_string(1),
    Str2 = which_string(2),
    Options = [{Str1, Str1}, {Str2, Str2}],
    {
        fun() -> wf:replace(unicode, #dropdown{id=unicode2, selected=Str1, options=Options}) end,
        fun() -> wf:q(unicode2)==Str1 end
    }.

set_unicode_dropdown_raw() ->
	Str = which_string(2),
	{
		fun() -> wf:set(unicode2, Str) end,
		fun() -> wf:q(unicode2)==Str end
	}.

