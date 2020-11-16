% vim: ts=4 sw=4 et
-module (index).
-include_lib ("nitrogen_core/include/wf.hrl").
%-compile({parse_transform, nitrogen_precompile}).
-compile(export_all).

main() -> #template { file="./templates/grid.html" }.

title() -> "Nitrogen Web Framework for Erlang".


layout() ->
    #container_12 { body=[
        common:github_fork(),
        #grid_12 { class=header, body=common:header(home) },
        #grid_clear {},

        #grid_6 { alpha=true, body=top_left() },
        #grid_6 { omega=true, body=top_right() },
        #grid_clear {},

        #grid_12 { body=common:footer() }
    ]}.

top_left() ->
    [
        #image { class=green_head, image="./images/green_head.png" }
    ].

top_right() ->
    [
        #p { class="summary", body=[
            "
            <b>Nitrogen Web Framework</b> is the fastest way to
            develop interactive web applications in full-stack Erlang.
            "
        ]},
        #p { class="section_title", body=""},
        #p { class="section section_download", body=[
            #link { url="/downloads", class="big_download_button", body=[
                <<"Download Nitrogen <u>&#9660;</u>">>
            ]},
            #br{},
            #link {url="/downloads", body=[
                #image { image="/images/downloads/erlang_logo.png" },
                #image { image="/images/downloads/mac_logo.png" },
                #image { image="/images/downloads/linux_logo.png" },
                #image { image="/images/downloads/windows_logo.png" },
                #image { image="/images/downloads/freebsd_logo.png" },
                #image { image="/images/downloads/raspberrypi_logo.png" }
            ]}
        ]},

        #p { class="section_title", body="MAJOR ANNOUNCEMENTS" },
        #p { class=section, body=[
            #time{ class=section_date, text="November 16th, 2020", datetime="2020-11-16"},
            #br{},
            <<"
            <b>Nitrogen 2.4.0 Released!</b><br>
            View the <a href='https://github.com/nitrogen/nitrogen/blob/master/CHANGELOG.markdown'>changelog</a>,
            or <b><a href='/downloads'>Download it now &raquo;</a></b>
            ">>,
            #br{},#br{},
            <<"
            <a href='http://builditwith.com/nitrogen'>Build it with Nitrogen: The fast-off-the-block Erlang web framework has been released!
                <img src='/images/book-3d.jpg'>
            </a>
            ">>
        ]},
        #p { class=section_title, body="MORE NITROGEN MEDIA"},

        #p { class=section, body=[
            <<"
            <a href='https://mostlyerlang.wordpress.com/2014/11/05/047-nitrogen-web-framework/'>Mostly Erlang Podcast Episode about Nitrogen</a>
            ">>
        ]},
        #p { class=section, body=[
            <<"
            Watch Introduction to Nitrogen from the Chicago Erlang Conference 2014!<br>
            <a href='https://www.youtube.com/watch?v=nlV4gm8SpVA'>Watch the video</a> or
            <a href='http://slides.sigma-star.com/view/ChicagErlangConference-Nitrogen#/'>View the slides</a>.
            ">>
        ]},
        #iframe{width=420, style="position:relative;", height=315, src="https://www.youtube-nocookie.com/embed/nlV4gm8SpVA", frameborder=0, allowfullscreen=true},
        #p { class="section_title", body="LATEST TWEETS" },
        #panel{class=twitter_button_wrapper,body=[
            <<"<a href=\"https://twitter.com/nitrogenproject\" data-show-count=\"false\" class=\"twitter-follow-button\" data-dnt=\"true\">Follow @nitrogenproject</a>
            <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0];if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=\"//platform.twitter.com/widgets.js\";fjs.parentNode.insertBefore(js,fjs);}}(document,\"script\",\"twitter-wjs\");</script>">>
        ]},

        <<"<a class=\"twitter-timeline\" height=400 data-tweet-limit=3 data-aria-polite=\"assertive\" data-chrome=\"noborders noheader nofooter transparent\" href=\"https://twitter.com/nitrogenproject\" data-widget-id=\"368136432696586240\">Tweets by @nitrogenproject</a>
        <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+\"://platform.twitter.com/widgets.js\";fjs.parentNode.insertBefore(js,fjs);}}(document,\"script\",\"twitter-wjs\");</script>">>
    ].
