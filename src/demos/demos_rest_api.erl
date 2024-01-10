%% -*- mode: nitrogen -*-
-module (demos_rest_api).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-behavior(nitrogen_rest).
 
get("") -> 
    Resp = io_lib:format("A GET request has been submitted with the parameters: ~p~n", [wf:params()]),
    {200, Resp}.

post("") ->
    %% The best way to post data is via the request body funcation
    Params = wf:request_body(),
    JsonPayload = wf:json_decode(Params),
    Body = io_lib:format("A POST request has been submitted with the parameters: ~p~n", [JsonPayload]),
    {200, Body}.

put("") ->
    Body = io_lib:format("A PUT request has been submitted with the parameters: ~p~n", ["N/A"]),
    {200, Body}.

delete("") ->
    Body = io_lib:format("A DELETE request has been submitted with the parameters: ~p~n", ["N/A"]),
    {200, Body}.
