-module(nitrogen_main_handler).
-export([
        run/0,
        ws_init/0
    ]).

handlers() ->
    ok.
    %nitrogen:global_handler(debug_crash_handler, []).
    %nitrogen:handler(website_config_handler, []).

ws_init() ->
    handlers(). 

run() ->
    handlers(),
    wf_core:run().
