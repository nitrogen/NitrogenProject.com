-module(nitrogen_website_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    nitrogen_website_sup:start_link().

stop(_State) ->
    ok.
