-module(tests_unicode_template).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() ->
	wf_test:start(fun tests/0),
	#template{file="priv/templates/unicode-test.html"}.

tests() ->
	?wf_test_js(unicode_template_text, unicode_template("text")),
	?wf_test_js(unicode_template_html, unicode_template("html")),
	ok.

unicode_template(Getter) ->
	{
		undefined,
		"return $('#value')." ++ Getter ++ "();",
		fun([V]) -> V == "Δοκιμή Πρότυπο" end
	}.
