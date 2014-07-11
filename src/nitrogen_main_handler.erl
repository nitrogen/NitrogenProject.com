-module(nitrogen_main_handler).
-export([
		run/0
	]).

run() ->
    nitrogen:handler(website_config_handler, []),
    nitrogen:run().
