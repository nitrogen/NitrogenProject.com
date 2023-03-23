% vim: ts=4 sw=4 et
-module(common).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

%template(Filename) ->
%    #template{file=template_location(Filename)}.

%% This produces a location like:
%% "priv/templates/Filename.html"
%% Except "priv" will be relative to the nitrogen_website application
template_location(Filename) ->
    filename:join([code:priv_dir(nitrogen_website),templates,Filename]).

platform() ->
    UA = wf:header(user_agent),
    %% Below is a hack for quickstart so it diesn't rely on having the useragent dep.
    Parsed = case erlang:function_exported(useragent, parse, 1) of
        true -> useragent:parse(UA);
        false -> [{os, [{family, linux}]}]
    end,
    OS = proplists:get_value(os, Parsed, []),
    _Platform = proplists:get_value(family, OS).


header(Selected) ->
    wf:wire(Selected, #add_class { class=selected }),
    #panel { class=menu, body=[
        #link { id=home, url="/", text="HOME" },
        #link { id=downloads, url="/downloads", text="DOWNLOADS" },
        #link { id=source, url="https://github.com/nitrogen", text="GITHUB"},
        #link { id=demos, url="/demos", text="DEMOS" },
        #link { id=docs, url="/doc/index", text="DOCUMENTATION" },
        #link { id=learn, url="/learn", text="LEARN MORE" },
        #link { id=community, url="/community", text="GET INVOLVED" }
    ]}.

footer() ->
    {Year,_,_} = date(),
    YearStr = integer_to_list(Year),
    #panel { class=credits, body=[
        "
        Copyright &copy; 2008-",YearStr," <a href='http://www.nitrogenproject.com'>Nitrogen Web Framework</a>. 
        <img src='/images/MiniSpaceman.png' style='vertical-align: middle;' />
        Released under the MIT License.
        "
    ]}.

google_analytics() ->
    <<"<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src=\"https://www.googletagmanager.com/gtag/js?id=G-6ZH5VMQ3L8\"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  gtag('config', 'G-6ZH5VMQ3L8');
</script>">>.

github_fork() ->
    [].

%    Body = #image{
%                image="https://s3.amazonaws.com/github/ribbons/forkme_left_red_aa0000.png",
%                style="position:absolute; top:0; left: 0; border: 0;z-index:1000",
%                alt="Fork me on GitHub"
%            },
%    #link{
%        style="position:absolute;top:0; left:0;",
%        url="https://github.com/nitrogen",
%        body=Body
%    }.


start_profiling(Secs) when is_integer(Secs) ->
    Filename = "nitrogen_trace_" ++ qdate:to_string("Y-m-d-H-i-s"),
    eep:start_file_tracing(Filename, [], nitrogen_modules()),
    timer:sleep(Secs*1000),
    eep:stop_tracing(),
    eep:convert_tracing(Filename),
    io:format("When the file is finished being processed, open with:\n\n   common:view_profile(\"" ++ Filename ++ "\").\n\n\n").

view_profile(Filename) ->
    spawn(fun() ->
        os:cmd("grep -v \"^ob=\" callgrind.out." ++ Filename ++ " > callgrind.out.merged_" ++ Filename),
        os:cmd("rm callgrind.out." ++ Filename),
        os:cmd("kcachegrind callgrind.out.merged_" ++ Filename)
    end).

nitrogen_modules() ->
    {ok, Mods} = application:get_key(nitrogen_core, modules),
    Mods.
