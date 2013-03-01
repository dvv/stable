-module(cowboy_cookie_session).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2, get_session/2, set_session/3]).

execute(Req, Env) ->

  % session_opts tuple is required in Env
  {session_opts, CookieOpts} = lists:keyfind(session_opts, 1, Env),

  % get session from request
  {Session, Req2} = get_session(CookieOpts, Req),

  % extract handler options
  {handler_opts, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
  % put session and session setter to handler options
  HandlerOpts2 = [{session, Session, CookieOpts} | HandlerOpts],
  % update handler_opts
  Env2 = [{handler_opts, HandlerOpts2} | lists:keydelete(handler_opts, 1, Env)],

  % set new environment
  {ok, Req2, Env2}.

%% -----------------------------------------------------------------------------
%% Get session from cookie.
%% -----------------------------------------------------------------------------

-spec get_session(Opts :: tuple(), Req :: binary()) ->
    {Session :: term(), Req :: binary()}.

get_session({Name, Secret, MaxAge, _Path}, Req) ->
  % get cookie
  {Cookie, Req2} = cowboy_req:cookie(Name, Req),
  % deserialize session
  Session = case termit:decode_base64(Cookie, Secret, MaxAge) of
    {error, _Reason} ->
      undefined;
    {ok, undefined} ->
      undefined;
    {ok, Sess} ->
      Sess
  end,
  {Session, Req2}.

%% -----------------------------------------------------------------------------
%% Set session to cookie, options explicitly passed.
%% -----------------------------------------------------------------------------

-spec set_session(Session :: term(), Opts :: tuple(), Req :: binary()) ->
    Req2 :: binary().

set_session(undefined, {Name, _Secret, _MaxAge, Path}, Req) ->
  % write already expired cookie
  cowboy_req:set_resp_cookie(Name, <<>>,
      [http_only, {max_age, 0}, {path, Path}], Req);

set_session(Session, {Name, Secret, MaxAge, Path}, Req) ->
  Cookie = termit:encode_base64(Session, Secret),
  cowboy_req:set_resp_cookie(Name, Cookie,
      [http_only, {max_age, MaxAge}, {path, Path}], Req).
