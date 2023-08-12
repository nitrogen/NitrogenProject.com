-module (linecount).
-include_lib ("nitrogen_core/include/wf.hrl").
-export ([render/0, render/1, render/2]).

render() ->
    CurrentModule = wf:page_module(),
    render("View This Page's Source", CurrentModule).

render(Module) ->
    render("View Source", Module).

render(CallToAction, Module) ->
     {_CurrentTotal, CurrentActive} = line_count(Module),
     %{_BareTotal, BareActive} = line_count(demos_barebones),
     ActiveLines = CurrentActive,% - BareActive,
     %ActiveLines = 20,

    [
%%         #p{},
%%         #span { class=stats, text=wf:f("This page clocks in at <span class=count>~p</span> lines of Nitrogen code.", [ActiveLines]), html_encode=false },
%%         #p{},
        #p{},
        #link{
            body=[CallToAction, #br{}, wf:f("(Module: ~p. ~p lines)",[Module, ActiveLines])],
            url=wf:f("/demos/viewsource?module=~s", [Module])
        }
    ].

    

line_count(Module) ->
    CompileInfo = Module:module_info(compile),
    Source = proplists:get_value(source, CompileInfo),
    {ok, B} = file:read_file(Source),
    String = wf:to_unicode_list(B),
    Lines = string:split(String, "\n", all),
    AllLines = length(Lines),
    ActiveLines = count_active_lines(Lines),
    {AllLines, ActiveLines}.

count_active_lines(Lines) ->
    lists:foldl(fun(Line, Acc) ->
        case only_whitespace(Line) of
            true -> Acc;
            false -> Acc + 1
        end
    end, 0, Lines).

-define(IS_WHITESPACE(X),
        X == $\s
        orelse X == $\t
        orelse X == $\r).

only_whitespace([]) ->
    true;
only_whitespace([$%|_]) ->
    true; %% short-circuit if we encounter a line that is just a comment
only_whitespace([H|T]) when ?IS_WHITESPACE(H) ->
    only_whitespace(T);
only_whitespace(_) ->
    false.

