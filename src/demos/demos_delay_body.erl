-module (demos_delay_body).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos46.html") }.

title() -> "Delayed Content".

headline() -> "Delayed Content".

left() -> 
    [
        "
        <p>
        The <code>#delay_body{}</code> element allows you to quickly render the
        page and leave a placeholder for content that might be slower to load.
        This is particularly useful for content that is either slow to process
        such as querying some external API, or for greater content that might
        be cached, but you want user-specific content to show within the cache.
        <p>
        This will require a <code>delay_body_event(Tag)</code> function to be
        defined in your page module (or the <code>delegate</code> module if so
        specified) and it must handle the <code>tag</code> specified with the
        <code>#delay_body</code> element.
        <p>
        The return value of this function will be placed into the content's page.
        <p>
        All these <code>delay_body_event</code> calls are performed in serial,
        and retrieved via postback. If the content to retrieve is expected to
        be <i>very</i> slow, or you simply need to ensure all results are
        retrieved or processed in parallel then the content is recommended to
        be loaded with <a href='/demos/continuations'>Continuations</a>, as
        each continuation will update when the content is available, rather
        than being handled in serial.
        ",
        linecount:render()
    ].

right() -> 
    [
        #h2{text="Slow-Loading Content"},
        "The system time is ",#delay_body{placeholder="(loading)", tag=slow_loading},

        #h2{text="Working with Cached Content"},
        do_cached_content()
    ].

do_cached_content() ->
    %% Here we pull the pre-rendered elements and actions from the cache
    {ok, Html, Actions} = wf:cache(delay_body_cache_demo, 60000, fun() ->
        %% If the cache is expired (we have a 60 second life defined) or not present,
        %% Then we retrive the content to cache
        Content = content_to_cache(),
        %% and pre-render it. This function call returns {ok, Html, Actions}.
        wf:render_isolated(Content)
    end),
    %% Now we wire the trapped actions
    wf:wire(Actions),
    %% and return the rendered HTML.
    Html.

content_to_cache() ->
    [
        "Here we have some content that is being cached. At the the time this
        was cached, the system time was <b>",qdate:to_string("Y-m-d g:i:sa"),"</b>.
        You can verify this by refreshing the page immediately, and it should
        roughly remain the same.

        But the content right here will be specific to you and you alone or is highly dynamic, so
        it <i>must not</i> be cached:
        <p>
        Your Session ID is: <b>",#delay_body{placeholder="(Loading...)", tag=session_id},"</b>",
        #br{},
        "The current live system time is: <b>",#delay_body{placeholder="XX:XX:XX", tag=system_time},"</b>"
    ].

delay_body_event(slow_loading) ->
    %% sleep for 500 ms
    timer:sleep(500),
    %% The return the current system time
    qdate:to_string("Y-m-d g:i:sa");
delay_body_event(session_id) ->
    wf:session_id();
delay_body_event(system_time) ->
    qdate:to_string("Y-m-d g:i:sa").

