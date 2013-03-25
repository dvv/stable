%%% ----------------------------------------------------------------------------
%%%
%%% @doc Provide quick coding patterns for some common use cases by delegating
%%%      actual processing to a helper module which exposes
%%%      `action(Method, Path, Req)` function.
%%%
%%% @doc The following cases covered:
%%%      - status, headers and body known
%%%      - status known
%%%      - redirect to new location
%%%      - render a module to HTML with some data
%%%
%%% ----------------------------------------------------------------------------

-module(cowboy_common_handler).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([init/3, terminate/3, handle/2]).

init(_Transport, Req, Opts) ->
  % apply apigen pragmatic REST recommendations
  {ok, cowboy_patch:patch_pragmatic_rest(
       cowboy_patch:patch_method(
       cowboy_patch:patch_headers(Req))),  Opts}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, State) ->
  % get request method and path
  [Method, Path] = cowboy_req:get([method, path_info], Req),

  % call handler
  {_, Handler} = lists:keyfind(handler, 1, State),
  Req2 = Req,
  case Handler:action(Method, Path, Req2) of
    % handler already responded
    {ok, Req3} ->
      {ok, Req3, undefined};
    % status, headers, body known
    {Status, Headers, Body, Req3} when is_integer(Status) ->
      {ok, Req4} = cowboy_req:reply(Status, Headers, Body, Req3),
      {ok, Req4, undefined};
    % shortcut response: status known, no body
    {Status, Req3} when is_integer(Status) ->
      {ok, Req4} = cowboy_req:reply(Status, Req3),
      {ok, Req4, undefined};
    % redirect and new location
    {redirect, Location, Req3} ->
      {ok, Req4} = cowboy_req:reply(302, [
          {<<"location">>, Location}
        ], <<>>, Req3),
      {ok, Req4, undefined};
    % template module and variables for interpolation
    {render, TemplateModule, Data, Req3} ->
      {ok, IoList} = TemplateModule:render(Data),
      {ok, Req4} = cowboy_req:reply(200, [], IoList, Req3),
      {ok, Req4, undefined}
  end.
