-module(demos_coalesce).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).
-compile({parse_transform, wf_lazy_coalesce}).

%% The name of the comet pool we'll be sending status updates
-define(POOL, status).

%% The values of the items in the array will be 
%% [?A, ?B, ?C, ?D]
%% With each of the above macros replaced with:
%% [undefined, "", 3.14, <<"another value">>]

-define(A, undefined).
-define(B, "").
-define(C, 3.14).
-define(D, <<"">>).
-define(E, some_atom).

main() ->
    #template{file=common:template_location("demos46.html")}.

title() -> "Coalesce Functions".

headline() -> "Coalesce Functions".

left() ->
    [
        "Nitrogen 3+ offers three different mechanisms for coalescing lists.
        By <i>coalescing</i> we mean to iterate through a list and return the
        first non-empty, non-blank, non-null item (that is, the following items
        will be skipped over: <code>\"\", <<>>, undefined</code>).
    
        <ul>
        <li><code>wf:coalesce/1</code> is the original.  It takes a list of items, and returns the first non-empty.</li>
        <li><code>wf:eval_coalesce/1</code> is a variant of <code>coalesce</code>
            that takes a list of functions with arity 0 (that is, functions that take
            no arguments), and returns the first non-empty value.  This can be
            perferred over <code>wf:coalesce/1</code> in case items in the list are
            slow, longer-running functions (say, API calls, for example).</li>
        <li><code>wf:lazy_coalesce/1</code> uses some Erlang trickery to combine
            the two methods above.  <code>wf:lazy_coalesce/1</code> must take an
            explicitly defined list.<br>
            For example:<br>
            This works: <code>wf:lazy_coalesce([fun1(), fun2()])</code><br>
            This does not: <code>L = [fun1(), fun2()], wf:lazy_coalesce(L)</code><br><br>
            This relies on some Erlang trickery in order to work this. At compile-time,
            Nitrogen will wrap the calls to <code>fun1()</code> and <code>fun2()</code>
            with anonymous functions (e.g. <code>fun() -> fun1() end</code>) then convert
            `wf:lazy_coalesce/` to `wf:eval_coalesce/1`.  This purpose of this is to make
            a clean way to add an optimized list without having to wrap everything in
            functions for optimization.<br>
            It's also smart enough to recognize that basic terms (atoms, strings, etc),
            do not have to be wrapped in functions, and that doing so is unnecessary overhead.</li>
        </ul>"
    ].

right() ->
    start_status_loop(),
    [
        #button{
            text="Start Demo",
            postback=start
        },
        #br{},
        "When this starts, it will process the same list of elements (",
        #code{text=wf:f("~p", [[?A, ?B, ?C, ?D, ?E]])},
        ") through three different variations on ",#code{text="coalesce"},". ",
        "The differences between each are explained on the left pane.",
        #singlerow{style="width:100%", cells=[
            #tablecell{body=[
                #code{text="coalesce"},
                #panel{id=standard}
            ]},
            #tablecell{body=[
                #code{text="eval_coalesce"},
                #panel{id=eval}
            ]},
            #tablecell{body=[
                #code{text="lazy_coalesce"},
                #panel{id=lazy}
            ]}
        ]}
    ].


event(start) ->
    run(standard),
    run(eval),
    run(lazy).


run(ID = standard) ->
    Fun = fun() ->
        wf:coalesce([
            slow(ID, ?A),
            slow(ID, ?B),
            slow(ID, ?C),
            slow(ID, ?D),
            slow(ID, ?E)
        ])
    end,
    run(ID, Fun);

run(ID = eval) ->
    Fun = fun() ->
        wf:eval_coalesce([
            fun() -> slow(ID, ?A) end,
            fun() -> slow(ID, ?B) end,
            fun() -> slow(ID, ?C) end,
            fun() -> slow(ID, ?D) end,
            fun() -> slow(ID, ?E) end
        ])
    end,
    run(ID, Fun);

run(ID = lazy) ->
    Fun = fun() ->
        wf:lazy_coalesce([
            slow(ID, ?A),
            slow(ID, ?B),
            slow(ID, ?C),
            slow(ID, ?D),
            slow(ID, ?E)
        ])
    end,
    run(ID, Fun).


run(ID, Fun) ->
    wf:comet(fun() ->
        clear(ID),
        {USec, Res} = timer:tc(Fun),
        MSec = USec div 1000,
        add_status(ID, wf:f("Finished (~pms)",[MSec])),
        add_status(ID, ["Return: ", #code{text=wf:f("~p", [Res])}])
    end).


slow(ID, X) ->
    add_status(ID, ["", #code{text=wf:f("~p",[X])}]),
    timer:sleep(1000),
    X.



%% Below is the comet loop that will update the user with what is happening

start_status_loop() ->
    wf:comet(fun comet_loop/0, ?POOL).

comet_loop() ->
    receive
        {clear, ID} ->
            wf:update(ID, "");
        {add, ID, Body} ->
            wf:insert_bottom(ID, #panel{body=Body})
    end,
    wf:flush(),
    comet_loop().

clear(ID) ->
    wf:send(?POOL, {clear, ID}).

add_status(ID, Text) ->
    wf:send(?POOL, {add, ID, Text}).
