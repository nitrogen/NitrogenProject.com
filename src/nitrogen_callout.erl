-module(nitrogen_callout).
-behaviour(simple_bridge_callout).
-export([
		run/1
	]).

run(Bridge) ->
    nitrogen:init_request(Bridge),
    %% Uncomment for basic authentication...
    nitrogen:handler(website_config_handler, []),
    nitrogen:run().
