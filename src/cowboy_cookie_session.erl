%%% ----------------------------------------------------------------------------
%%%
%%% @doc Secure signed encrypted cookie session adapter for
%%%      cowboy_session middleware.
%%%
%%% 1. Insert `cowboy_session` middleware somewhere between `cowboy_router` and
%%%    `cowboy_handler`
%%%
%%% 2. Add session options to request environment, e.g.
%%%
%%%    {session_opts, {cowboy_cookie_session, {
%%%         <<"sid">>,       % cookie name
%%%         <<"tOpcekpet">>, % encryption secret
%%%         1000,            % cookie time-to-live in seconds
%%%         <<"/">>}}}       % cookie path
%%%
%%% ----------------------------------------------------------------------------

-module(cowboy_cookie_session).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    get/2,
    set/3,
    drop/2
  ]).

%% -----------------------------------------------------------------------------
%% Get session from cookie.
%% -----------------------------------------------------------------------------

get({Name, Secret, _MaxAge, _Path}, Req) ->
  % get cookie
  {Cookie, Req2} = cowboy_req:cookie(Name, Req),
  % deserialize session
  Session = case termit:verify_token(Cookie, Secret) of
    {error, _Reason} ->
      undefined;
    % {ok, undefined} ->
    %   undefined;
    {ok, Sess} ->
      Sess
  end,
  {Session, Req2}.

%% -----------------------------------------------------------------------------
%% Store session in cookie.
%% -----------------------------------------------------------------------------

set(undefined, {Name, _Secret, _MaxAge, Path}, Req) ->
  % write already expired cookie
  cowboy_req:set_resp_cookie(Name, <<>>,
      [http_only, {max_age, 0}, {path, Path}], Req);

set(Session, {Name, Secret, MaxAge, Path}, Req) ->
  Cookie = termit:issue_token(Session, Secret, MaxAge),
  cowboy_req:set_resp_cookie(Name, Cookie,
      [http_only, {max_age, MaxAge}, {path, Path}], Req).

%% -----------------------------------------------------------------------------
%% Drop session cookie.
%% -----------------------------------------------------------------------------

drop(Opts, Req) ->
  set(undefined, Opts, Req).
