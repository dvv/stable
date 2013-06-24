%%% ----------------------------------------------------------------------------
%%%
%%% @doc Sessions for cowboy.
%%%
%%% 1. Insert `cowboy_session` middleware somewhere between `cowboy_router` and
%%%    `cowboy_handler`
%%%
%%% 2. Add session options to request environment:
%%%
%%%    {session_opts, {SessionImplementationModuleName, ImplementationOptions}}
%%%
%%%    E.g.:
%%%
%%%    {session_opts, {cowboy_cookie_session, {
%%%         <<"sid">>,       % cookie name
%%%         <<"tOpcekpet">>, % encryption secret
%%%         1000,            % cookie time-to-live in seconds
%%%         <<"/">>}}}       % cookie path
%%%
%%% 3. Access, modify or drop session:
%%%
%%%    {Session, Req2} = cowboy_session:get(Req),
%%%    Req3 = cowboy_session:set(NewSession, Req2),
%%%    Req4 = cowboy_session:drop(Req3).
%%%
%%% NB: by default setting session is idempotent in sense action is taken only
%%%    if new session differs from already stored one.
%%%    To unconditionally set new session pass `true` as the third parameter to
%%%    `set/3`.
%%%
%%% ----------------------------------------------------------------------------

-module(cowboy_session).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2]).

-export([
    drop/1,
    get/1,
    set/2,
    set/3
  ]).

execute(Req, Env) ->
  % NB: session_opts in looked up in handler options or request environment
  % whichever is found first
  OptionsSource = case lists:keyfind(handler_opts, 1, Env) of
    {_, HandlerOpts} ->
      case lists:keymember(session_opts, 1, HandlerOpts) of
        true -> HandlerOpts;
        false -> Env
      end;
    false ->
      Env
  end,
  case lists:keyfind(session_opts, 1, OptionsSource) of
    {_, {Mod, Opts}} ->
      {Session, Req2} = Mod:get(Opts, Req),
      {ok, cowboy_req:set_meta(session, {Session, Mod, Opts}, Req2), Env};
    false ->
      {ok, Req, Env}
  end.

%% -----------------------------------------------------------------------------
%% Get session.
%% -----------------------------------------------------------------------------

get(Req) ->
  case cowboy_req:meta(session, Req) of
    {{Session, _, _}, Req2} ->
      {Session, Req2};
    {undefined, Req2} ->
      {undefined, Req2}
  end.

%% -----------------------------------------------------------------------------
%% Set session.
%% -----------------------------------------------------------------------------

%% alias for set(Session, Req, false)
set(Session, Req) ->
  set(Session, Req, false).

% NB: set only if new session differs from old session or Override is true
set(Session, Req, Override) ->
  case cowboy_req:meta(session, Req) of
    {{Session, _, _}, Req2} when not Override ->
      Req2;
    {{_, Mod, Opts}, Req2} ->
      Req3 = Mod:set(Session, Opts, Req2),
      cowboy_req:set_meta(session, {Session, Mod, Opts}, Req3)
  end.

%% -----------------------------------------------------------------------------
%% Drop session.
%% -----------------------------------------------------------------------------

drop(Req) ->
  {{_, Mod, Opts}, Req2} = cowboy_req:meta(session, Req),
  Mod:drop(Opts, Req2).
