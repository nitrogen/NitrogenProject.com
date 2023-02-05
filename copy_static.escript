#!/usr/bin/env escript
%% -*- erlang -*-
%% vim: ts=4 sw=4 et ft=erlang
%%
%% Coyright 2022 - Jesse Gumm
%%
%% MIT License
%%
%% About
%%
%% This will copy or link the nitrogen static directory into the
%% priv/static/nitrogen directory based on where it finds the nitrogen_core
%% directory (checkouts or lib)

prefix(checkouts) ->
    "_checkouts/nitrogen_core";
prefix(build_checkouts) ->
    "_build/default/checkouts/nitrogen_core";
prefix(build_lib) ->
    "_build/default/lib/nitrogen_core".

possible_locations("static") ->
    [
        prefix(checkouts) ++ "/www", %% old loc, but let's check
        prefix(checkouts) ++ "/priv/www",
        prefix(build_checkouts) ++ "/priv/www",
        prefix(build_lib) ++ "/priv/www"
    ];
possible_locations("doc") ->
    Doc = "/doc/markdown",
    [
        prefix(checkouts) ++ Doc,
        prefix(build_checkouts) ++ Doc,
        prefix(build_lib) ++ Doc
    ].

dest("static") ->
    "priv/static/nitrogen";
dest("doc") ->
    "priv/static/doc".

main([]) ->
    main(["copy", "static"]);
main([Action]) ->
    main([Action, "static"]);
main([Action, Item])
        when (Action=="copy" orelse Action=="link") andalso 
             (Item=="static" orelse Item=="doc") ->
    PossibleLocations = possible_locations(Item),
    Src = find_first_loc(PossibleLocations),
    Dest = dest(Item),
    do_action(Action, Src, Dest).

do_action(Action, Src, Dest) ->
    cmd("rm -fr " ++ Dest),
    case Action of
        "copy" ->
            cmd("mkdir -p " ++ Dest),
            cmd("cp -r " ++ Src ++ "/* " ++ Dest);
        "link" ->
            cmd("ln -s `pwd`/" ++ Src ++ " " ++ Dest)
    end.
        

cmd(Cmd) ->
    io:format("Running: ~s~n", [Cmd]),
    os:cmd(Cmd).


find_first_loc([]) ->
    exit("nitrogen_core directory not found to link the www directory");
find_first_loc([H|T]) ->
    case filelib:is_dir(H) of
        true -> H;
        false -> find_first_loc(T)
    end.
    
