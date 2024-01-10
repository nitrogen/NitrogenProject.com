-module(nitrogen_main_handler).
-include_lib("nitrogen_core/include/wf.hrl").
-export([
        run/0,
        ws_init/0
    ]).

handlers() ->
    %nitrogen:handler(debug_crash_handler, []),
    ok.

ws_init() ->
    handlers(). 

run() ->
    handlers(),
    wf_core:run().
    %?WF_PROFILE(run, wf_core:run(), csv_path()).
   
csv_path() ->
    filename:join(home_dir(), profile_file()).

profile_file() ->
    DateTime = qdate:to_string("Y-m-d-h-i-s"),
    "nitrogen_profile-" ++ DateTime ++ ".csv".

home_dir() ->
    {ok, [[Home]]} = init:get_argument(home),
    Home.
