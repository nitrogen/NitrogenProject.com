%% -*- mode: nitrogen -*-
-module(nitrogen_website_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Start the Process Registry...
	application:start(crypto),
    application:ensure_all_started(nitro_cache),
    application:start(nprocreg),
	application:start(simple_bridge),

    {ok, { {one_for_one, 5, 10}, []} }.

