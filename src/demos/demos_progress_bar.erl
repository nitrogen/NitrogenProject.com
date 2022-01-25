% vim: ts=4 sw=4 et
-module (demos_progress_bar).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos433.html") }.

title() -> "Progress Bars".
headline() -> "Progress Bars".

left() -> 
    [
        "
        <p>
        The <code>#progress_bar{}</code> gives you a convenient way to add
        simple progress bars to your application which can be interacted with
        through Nitrogen's standard methods.",
        progress_bar_js(),
        linecount:render()
    ].

progress_bar_js() ->
    %% This is only used by the "progress bar updated client-side only"
    <<"<script>
        function start_progress_bar(id) {
            objs('start_client_button').hide();
            update_progress_bar(id, 0);
        }

        function update_progress_bar(id, value) {
            Nitrogen.$set_value(page, id, value);
            if(value < Nitrogen.$get_progress_bar_max(id))
                setTimeout(function() { update_progress_bar(id, value+1)}, 10);
            else
                objs('start_client_button').show();     
        }
    </script>">>.

middle() ->
    [
        #h3{text="Simple" },
        #progress_bar{value=30},

        #h3{text="Undefined Value"},
        #progress_bar{value=undefined},

        #h3{text="Custom Color" },
        #progress_bar{value=70, color="700"},

        #h3{text="Random Colors and Custom Label Types"},
        #progress_bar{value=?WF_RAND_UNIFORM(1, 1337), color=random_color(), max=1337, label=ratio},
        #br{},
        #progress_bar{value=?WF_RAND_UNIFORM(1, 1337), color=random_color(), max=1337, label=percent},
        #br{},
        #progress_bar{value=?WF_RAND_UNIFORM(1, 1337), color=random_color(), max=1337, label=both},
        #br{},
        #progress_bar{value=?WF_RAND_UNIFORM(1, 1337), color=random_color(), max=1337, label="A custom label"}
    ].

right() ->
    [
        #h3{text="Updated server-side"},
        #progress_bar{id=comet_progress_bar, value=0, label=percent},
        #button{id=start_server_button, text="Start Server-Side Updating", postback=start_server_updating},

        #h3{text="Updated client-side only"},
        #progress_bar{id=js_progress_bar, value=0, max=400, label=both},
        #button{id=start_client_button, text="Start Client-Side Updating", click="start_progress_bar('js_progress_bar')"}
    ].


event(start_server_updating) ->
    wf:wire(start_server_button, #hide{}),
    wf:comet(fun() -> progress_bar_loop(random_delay(), 0) end).

random_color() ->
    {ok, Color} = wf:hex_encode([?WF_RAND_UNIFORM(0,256) || _ <- lists:seq(1,3)]),
    Color.

random_delay() ->
    %% random delay between 100 and 1000 miliseconds
    ?WF_RAND_UNIFORM(100, 500).

random_increment() ->
    %% And we'll randomly increment each comet step by somewhere between 1 and 20
    ?WF_RAND_UNIFORM(1, 20).

progress_bar_loop(Delay, Value) when is_integer(Delay), is_integer(Value) ->
    wf:set(comet_progress_bar, Value),
    wf:flush(),
    timer:sleep(Delay),
    case calculate_new_delay_and_value(Delay, Value) of
        done ->
            wf:wire(start_server_button, #show{});
        {NewDelay, NewValue} ->
            ?MODULE:progress_bar_loop(NewDelay, NewValue)
    end.

calculate_new_delay_and_value(_Delay, Value) when Value >= 100 ->
    done;
calculate_new_delay_and_value(Delay, Value) ->
    %% If not, then we add a random increment to get a new value (but no
    %% greater than 100), and we can keep the same Delay
    NewValue = lists:min([Value + random_increment(),100]),
    {Delay, NewValue}.
