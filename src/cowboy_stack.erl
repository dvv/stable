-module(cowboy_stack).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2]).

%%
%% Executes a series of functions listed in `stack` environment key on `Req`.
%%
execute(Req, Env) ->
  {_, Stack} = lists:keyfind(stack, 1, Env),
  {ok, stack(Stack, Req), Env}.

stack([], Req) ->
  Req;
stack([H | T], Req) when is_function(H) ->
  stack(T, H(Req));
stack([{M, F, A} | T], Req) ->
  stack(T, apply(M, F, [Req | A]));
%% NB: conditionally apply the layer
stack([{Prefix, M, F, A} | T], Req) ->
  {Path, Req2} = cowboy_req:path(Req),
  PSize = byte_size(Prefix),
  case Path of
    << Prefix:PSize/binary, _/binary >> ->
      stack(T, apply(M, F, [Req2 | A]));
    _ ->
      stack(T, Req2)
  end.
