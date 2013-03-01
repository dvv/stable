%%
%% @doc Delegates actual processing to a helper module
%%      which exposes `handler(Method, Path, Req, Session)`.
%%      Overwrites session if handler returns modified session.
%%
%% @doc Return values are matched against the common cases:
%%      - status, headers and body known
%%      - error status known
%%      - render a module to HTML with some data
%%      - response already answered
%%

-module(cowboy_common_handler).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([init/3, terminate/3, handle/2]).

init(_Transport, Req, Opts) ->
  {ok, Req, Opts}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, Opts) ->
  % get handler module
  {handler, Handler} = lists:keyfind(handler, 1, Opts),
  % get request method and path
  [Method, Path] = cowboy_req:get([method, path_info], Req),

  % get session
  {Session, SessionOpts, Req2} = case lists:keyfind(session, 1, Opts) of
    {session, Session1, SessionOpts1} ->
      {Session1, SessionOpts1, Req};
    false ->
      {undefined, undefined, Req}
  end,

  % call handler
  case Handler:handler(Method, Path, Req2, Session) of
    % handler already responded
    {ok, Req3} ->
      {ok, Req3, undefined};
    % status, headers, body known
    {Status, Headers, Body, Req3, Session2} when is_integer(Status) ->
      respond(Status, Headers, Body, Req3, Session, Session2, SessionOpts);
    {Status, Headers, Body, Req3} when is_integer(Status) ->
      respond(Status, Headers, Body, Req3);
    % shortcut response: status known, no body
    {Status, Req3, Session2} when is_integer(Status) ->
      respond(Status, [], <<>>, Req3, Session, Session2, SessionOpts);
    {Status, Req3} when is_integer(Status) ->
      respond(Status, [], <<>>, Req3);
    % redirect
    {redirect, Location, Req3, Session2} ->
      respond(303, [
          {<<"location">>, Location}
        ], <<>>, Req3, Session, Session2, SessionOpts);
    {redirect, Location, Req3} ->
      respond(303, [
          {<<"location">>, Location}
        ], <<>>, Req3);
    % template module and variables for interpolation
    {render, TemplateModule, Data, Req3, Session2} ->
      render(TemplateModule, Data, Req3, Session, Session2, SessionOpts);
    {render, TemplateModule, Data, Req3} ->
      render(TemplateModule, Data, Req3, false, false, undefined)
  end.

render(TemplateModule, Data, Req, Session, Session2, SessionOpts) ->
  try TemplateModule:render(Data) of
    {ok, IoList} ->
      respond(200, [
          {<<"content-type">>, <<"text/html; charset=UTF-8">>}
        ], IoList, Req, Session, Session2, SessionOpts);
    {error, _Error} ->
      respond(500, [], <<"Render error">>, Req)
  catch _:_ ->
    respond(500, [], <<"Render error">>, Req)
  end.

respond(Status, Headers, Body, Req) ->
  {ok, Req2} = cowboy_req:reply(Status, Headers, Body, Req),
  {ok, Req2, undefined}.

respond(Status, Headers, Body, Req, _, _, undefined) ->
  respond(Status, Headers, Body, Req);
respond(Status, Headers, Body, Req, Session, Session2, SessionOpts) ->
  Req2 = case Session2 of
    Session ->
      Req;
    _ ->
      cowboy_cookie_session:set_session(Session2, SessionOpts, Req)
  end,
  respond(Status, Headers, Body, Req2).
