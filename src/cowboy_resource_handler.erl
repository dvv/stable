-module(cowboy_resource_handler).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-type proplist() :: list({term(), term()}).
-type method() :: binary().
-type querystring() :: proplist().
-type body() :: proplist().
-type options() :: proplist().
-type location() :: binary().
-type result() :: term().
-type error() :: term().
-type scope() :: binary() | list(binary()).

%%
%% Given credentials and their type return opaque authorization term.
%%
-callback authorize(
    Type :: token | password | session,
    Credentials :: term(),
    Options :: options()) ->
  {ok, {Identity :: term(), Scope :: scope()}} |
  {error, Reason :: error()}.

%%
%% Given resource method to call and authorization term returned from
%% authorize/3 determine whether call to method is allowed.
%%
-callback allowed(
    Method :: method(),
    {Identity :: term(), Scope :: scope()}) ->
  true |
  false.

%%
%% Retrieve resource.
%%
%% Query is proplist collected from URI params.
%% Options is proplist options specified for handler in router augmented with
%%   authorization info obtained in authorize/3.
%%
-callback get(
    Query :: querystring(),
    Options :: options()) ->
  {ok, Result :: result()} |
  {goto, Where :: location()} |
  error |
  {error, enoent} |
  {error, Reason :: error()}.

%%
%% Create resource.
%%
%% Body is new resource entity.
%% Query is proplist collected from URI params.
%% Options is proplist options specified for handler in router augmented with
%%   authorization info obtained in authorize/3.
%%
-callback post(
    Body :: body(),
    Query :: querystring(),
    Options :: options()) ->
  ok |
  {ok, Result :: result()} |
  {goto, Where :: location()} |
  error |
  {error, eexist} |
  {error, Reason :: error()}.

%%
%% Replace resource.
%%
%% Body is resource entity.
%% Query is proplist collected from URI params.
%% Options is proplist options specified for handler in router augmented with
%%   authorization info obtained in authorize/3.
%%
-callback put(
    Body :: body(),
    Query :: querystring(),
    Options :: options()) ->
  ok |
  {ok, Result :: result()} |
  error |
  {error, eexist} |
  {error, Reason :: error()}.

%%
%% Update resource.
%%
%% Body is collection of changes.
%% Query is proplist collected from URI params.
%% Options is proplist options specified for handler in router augmented with
%%   authorization info obtained in authorize/3.
%%
-callback patch(
    Body :: body(),
    Query :: querystring(),
    Options :: options()) ->
  ok |
  {ok, Result :: result()} |
  error |
  {error, enoent} |
  {error, Reason :: error()}.

%%
%% Remove resource.
%%
%% Query is proplist collected from URI params.
%% Options is proplist options specified for handler in router augmented with
%%   authorization info obtained in authorize/3.
%%
-callback delete(
    Query :: querystring(),
    Options :: options()) ->
  ok |
  accepted |
  error |
  {error, enoent} |
  {error, Reason :: error()}.

%%
%% Handle RPC via POST to resource.
%%
%% Function is method to call.
%% Args is arguments proplist.
%% Options is proplist options specified for handler in router augmented with
%%   authorization info obtained in authorize/3.
%%
-callback call(
    Function :: binary(),
    Args :: list(),
    Options :: options())->
  {ok, Result :: result()} |
  {error, Reason :: error()}.
