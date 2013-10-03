% vim: ts=4 sw=4 et
-module (downloads).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

-define(UNIX_SERVERS,[
        {cowboy,"Cowboy"},
        {inets,"Inets"},
        {mochiweb,"Mochiweb"},
        {webmachine,"Webmachine"},
        {yaws,"Yaws"}
    ]).

-define(WINDOWS_SERVERS,[
        {cowboy,"Cowboy"},
        {inets,"Inets"},
        {mochiweb,"Mochiweb"},
        {webmachine,"Webmachine"}
    ]).

-define(ALL_VERSIONS, [
        "2.2.0",
        "2.1.0",
        "2.0.4",
        "2.0.3",
        "2.0.2",
        "2.0.1",
        "2.0.0",
        "1.0"]).

-define(PLATFORMS, [
        {"linux", "Linux", ?UNIX_SERVERS, "64bit"},
        {"mac", "Mac OSX", ?UNIX_SERVERS, "64bit"},
        {"windows", "Windows", ?WINDOWS_SERVERS, "32bit"},
        {"freebsd", "FreeBSD", ?UNIX_SERVERS, "64bit"},
        {"raspberrypi", "Raspberry Pi (Raspbian)", ?UNIX_SERVERS, "64bit"}
    ]).

main() -> #template { file="./templates/grid.html" }.

title() -> "Downloads".

layout() -> 
    #container_12 { body=[
        common:github_fork(),
        #grid_12 { class=header, body=common:header(downloads) },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=headline, body=headline() },
        #grid_clear {},

        #grid_5 { prefix=1, alpha=true, body=left(), class=pad_right },
        #grid_5 { suffix=1, omega=true, body=right() },
        #grid_clear {},

        #grid_12 { body=common:footer() }
    ]}.

headline() -> "Downloads".

left() -> 
    Servers = [
        {"Cowboy", "https://github.com/extend/cowboy","High performance HTTP server developed by Loïc Hoguin."},
        {"Inets","http://www.erlang.org/doc/man/httpd.html","Lightweight HTTP server built into Erlang."},
        {"Mochiweb","https://github.com/mochi/mochiweb","Lightweight HTTP server developed by Bob Ippolito/MochiMedia."},
        {"Webmachine","http://webmachine.basho.com/","HTTP resource server developed by Basho Technologies (runs on Mochiweb under the hood.)"},
        {"Yaws","http://yaws.hyber.org/","A mature high-performance HTTP server developed by Claes \"Klacke\" Wikstrom."}
    ],

    [
        <<"
        Each platform download is a self-contained installation of Nitrogen
        that includes both Erlang and a web server. (In other words, you don't
        need to have Erlang installed to run this.)
        <p>
        You have a choice between five popular Erlang web servers:
        ">>,
        wf:join([#link{text=Name, url=Url} || {Name, Url, _Desc} <- Servers],", "),
        #br{},#br{},
        #link{
            class=server_help_toggle,
            body="Not sure which server to choose? &#9660;",
            click=#toggle{target=server_help, effect=fade}
        },
        #panel{id=server_help, class=[server_help, help], style="display:none",body=server_help()},
           
        <<"<p>
        These packages were generated from Nitrogen source code by running 
        <b>make package_inets</b>, <b>make package_cowboy</b>, 
        <b>make package_mochiweb</b>, <b>make package_webmachine</b> and 
        <b>make package_yaws</b>.
        <p>
        Alternatively, if you plan on contributing to the Nitrogen
        source code, you can download the source tree from GitHub.">>
    ].

server_help() ->
    [
    <<"<b>Cowboy</b> is the newest popular webserver on the Erlang scene.
    Created by Loic Hoguin, and under heavy development, it currently supports
    SPDY, WebSockets (which Nitrogen will soon support), and follows a model
    that differentiates itself from the other Erlang webservers: It doesn't
    maintain a process dictionary. Instead, it follows a more 'pure' flow, by
    passing around the a request object to each of Cowboy's functions. It's a
    lightweight and high performance webserver, and very capable.">>,
    #br{},
    #br{},
    <<"<b>Inets</b> is the built-in Erlang Webserver. It's lightweight, and
    gets the job done. Its biggest benefit is that it does not require any
    external dependencies, since it's a part of the standard Erlang release. If
    you have a simple app that doesn't require much by way of high performance,
    Inets will work.">>,
    #br{},
    #br{},
    <<"<b>Mochiweb</b> is a simple webserver that sticks mostly to maintenance
    releases these days, new features are no longer being added, so it seems
    Mochiweb will forever not support Websockets. But it is a perfectly
    performant server for most web application uses.">>,
    #br{},
    #br{},
    <<"<b>Webmachine</b> is a webserver with an emphasis on REST. It's main
    focus is for making great RESTful APIs. The perfect use-case for using
    Webmachine with Nitrogen would be to provide a powerful RESTful API for
    external apps (like mobile device APIs), while also providing an
    interactive interface utilizing Nitrogen for web users.">>,
    #br{},
    #br{},
    <<"<b>Yaws</b> is the granddaddy of Erlang webservers. It's tried and true,
    has been around for many years, and continues to improve, including adding
    Websocket support. It has solid performance metrics, and has a
    configuration system modelled after Apache's.">>,
    #br{},#br{},
    <<"<b>IN GENERAL</b> if you are completely unsure which webserver to use,
    we recommend choosing either Cowboy or Yaws for production environments, as
    both provide quality performance, support websockets (which Nitrogen is
    slated to support as of version 2.3), and both handle large files
    smoothly">>
    ].


%% Servers is tuple list = [{"Mochiweb",mochiweb},...]
list_download_links(PlatformLabel,PlatformPath,Version,Ext,Servers) ->
    [format_download_link(PlatformLabel,PlatformPath,Version,Ext,Server) || Server <- Servers].

format_download_link(PlatformLabel,PlatformPath,Version,Ext,{ServerPath,ServerName}) ->
    URL = wf:to_list([
        "http://downloads.nitrogenproject.com.s3.amazonaws.com",
        "/",Version,"/",PlatformPath,
        "/nitrogen-",Version,"-",ServerPath,Ext
    ]),
    Label = wf:to_list([
        "Nitrogen ",Version," for ",PlatformLabel," on ",ServerName
    ]),

    #link {
        class=link,
        url=URL,
        text=Label
    }.

list_source_download_links(Versions) ->
    [format_source_download_link(V) || V <- Versions].

format_source_download_link(Version) ->
    #link{
        class=link,
        url=wf:to_list(["http://github.com/nitrogen/nitrogen/tarball/v",Version]),
        text=wf:to_list(["Download Nitrogen ",Version," source (.tar.gz)"])
    }.

real_platform(Platform) when is_atom(Platform) ->
    real_platform(atom_to_list(Platform));
real_platform(Platform) ->
    case lists:keyfind(Platform,1,?PLATFORMS) of
        false -> "source";
        _ -> Platform
    end.

platform_dropdown(Default) ->
    RealDefault = real_platform(Default),
    Opts = 
        [#option{text="Choose Platform", value="choose"}]
        ++ [#option{text=[PlatformName," Binaries"], value=Platform}
            || {Platform, PlatformName, _, _} <- ?PLATFORMS],
    #dropdown{
        id=platform,
        value=RealDefault,
        options=Opts,
        postback=choose_platform
    }.

right() ->
    DefaultPlatform = real_platform(common:platform()),
    CurrentVersion = hd(?ALL_VERSIONS),
    [
        #h2 { text=["The Latest Version of Nitrogen: ",CurrentVersion]},
        "See what's new this version in ",
        #link {
            url="https://github.com/nitrogen/nitrogen/blob/master/CHANGELOG.markdown",
            text="The CHANGELOG",
            new=true
        },

        #h2 { text="Choose your platform for download"},
        #panel { class=download_dropdown_wrapper, body=[
            platform_dropdown(DefaultPlatform)
        ]},
        #br{},
        #panel { id=platform_downloads, body=platform_downloads(DefaultPlatform)},

        #panel { class=clear },
        #h2 { text="...or Download the Source Code"},
        platform_downloads("source"),
        #panel { class=clear },

        #panel { class=platform, body=[
            #panel { class=[logo,doc_logo], body=[
                #image { image="/images/downloads/documentation.png" }
            ]},
            #span { class=title, text="Nitrogen Documentation" },
            #link { class=link, url="/doc/index.html", text="View Documentation Online" },
            #link { class=link, url="/doc/tutorial.html", text="View the Nitrogen Tutorial" },
            "Docs are also included in platform downloads."
        ]},

        #panel{class=clear},
        platform_downloads("old")
    ].

platform_downloads("source") ->
    CurrentVersion = hd(?ALL_VERSIONS),
    #panel { class=platform, body=[
        #panel { class=logo, body=[
            #image { image="/images/downloads/erlang_logo.png" }
        ]},
        #span { class=title, text="Source Code" },
        list_source_download_links([CurrentVersion]),
        #link { url="https://github.com/nitrogen/nitrogen/tarball/master", text="Download Latest Code (.tar.gz)" },
        #link { url="https://github.com/nitrogen", text="Master Nitrogen repositories on GitHub" },
        #link { url="https://github.com/nitrogen/nitrogen/wiki/Nitrogen-Plugins",text="List of Nitrogen Plugins" },
        #link { url="https://github.com/RomanShestakov/nitrogen_elements", text="Community Repository of Nitrogen Elements" },
        #br{},
        <<"<b>Or clone with Git:</b>">>,
        <<"<pre style='margin-top:0.5em'>git clone git://github.com/nitrogen/nitrogen.git</pre>">>
    ]};
platform_downloads("old") ->
    OldVersions = tl(?ALL_VERSIONS),
    #panel { class=platform, body=[
        #panel { class=logo, body=[
            #image { image="/images/downloads/erlang_logo.png" }
        ]},
        #span { class=title, text="Old Versions of Nitrogen" },
        list_source_download_links(OldVersions)
    ]};
platform_downloads(Platform) ->
    case lists:keyfind(Platform,1,?PLATFORMS) of 
        false -> [];
        {_,PlatformName,Servers,Bits} ->
            CurrentVersion = hd(?ALL_VERSIONS),
            Suffix = ?WF_IF(Platform=="windows","-win.zip",".tar.gz"),
            [
                #panel { class=clear },

                #panel { class=platform, body=[
                    #panel { class=logo, body=[
                        #image { image=["/images/downloads/",Platform,"_logo.png"] }
                    ]},
                    #span { class=title, text=[PlatformName," Binaries"] },
                    list_download_links(PlatformName,Platform, CurrentVersion,Suffix,Servers),
                    <<"(includes Erlang compiled for ">>,PlatformName,<<"/">>,Bits,<<")">>
                ]}
            ]
    end.
                                                                                  
event(choose_platform) ->
    Platform = wf:q(platform),
    wf:update(platform_downloads, platform_downloads(Platform)).
