-module(nitrogen_callout).
-behaviour(simple_bridge_callout).
-export([
		run/1
	]).

run(Bridge) ->
    nitrogen:init_request(Bridge),

	%% This file needs to be moved to nitrogen_core, with a handler callback provided.
	%% Something like nitrogen_handlers, accompanied by perhaps a configuration file
	%% pointing to the M:F() call.  For now, we'll keep it here
    nitrogen:handler(website_config_handler, []),
    nitrogen:run().
