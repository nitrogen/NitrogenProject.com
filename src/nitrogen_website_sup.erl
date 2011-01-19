%% -*- mode: nitrogen -*-
-module(nitrogen_website_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1,
    do/1
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
    {ok, Port} = application:get_env(port),
    
    io:format("Starting NitrogenProject.com on port ~p.~n", [Port]),
    
    inets:start(),
    {ok, Pid} = inets:start(httpd, [
        {port, Port},
        {server_name, "nitrogen"},
        {server_root, "."},
        {document_root, "./static"},
        {modules, [?MODULE]},
        {mime_types, [{"css", "text/css"}, {"js", "text/javascript"}, {"html", "text/html"}]}
    ]),
    link(Pid),

    application:start(nprocreg),
    {ok, { {one_for_one, 5, 10}, []} }.

do(Info) ->
    RequestBridge = simple_bridge:make_request(inets_request_bridge, Info),
    ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info),
    nitrogen:init_request(RequestBridge, ResponseBridge),

    %% Uncomment for basic authentication...
    %% nitrogen:handler(http_basic_auth_security_handler, basic_auth_callback_mod),

    nitrogen:run().
