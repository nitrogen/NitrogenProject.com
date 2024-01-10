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
    application:ensure_all_started(nitrogen_core),
    application:start(crypto),
    application:ensure_all_started(nitro_cache),
    application:start(nprocreg),
    application:ensure_all_started(canister),
	application:start(simple_bridge),

    %% This is only used for an experimental feature. Keeping for now, since
    %% it's safe if the feature doesn't exist.
    %case erlang:function_exported(nitrogen, global_handler, 2) of
    %    true ->
    %        nitrogen:global_handler(debug_crash_handler, []);
    %    false ->
    %        ok
    %end,

    {ok, { {one_for_one, 5, 10}, []} }.

