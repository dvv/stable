%%
%% @doc Prepend URI with HTTP method used, to allow richer matching.
%%

-module(cowboy_mrouter).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
  [Method, Path] = cowboy_req:get([method, path], Req),
  {ok, cowboy_req:set([{path, <<$/, Method/binary, Path/binary>>}], Req), Env}.
