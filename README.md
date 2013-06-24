Stable
==============

Collection of assorted helpers for [Cowboy](https://github.com/extend/cowboy) web server.

cowboy_resource
--------------

Generic handler for RESTful resources. Supercedes `cowboy_rpc`.

Router configuration:
```erlang
%% in env key of protocol configuration:
{"/api/:bucket[/:id]", pecypc_test, [{token_secret, <<"!cowboyftw!">>}]},
```

Handler should be a module implementing `cowboy_resource_handler` behaviour:
```erlang
-module(pecypc_test).

-export([
    init/3
  ]).

-behaviour(cowboy_resource_handler).
-export([
    allowed/2,
    authorize/3,
    call/3,
    delete/2,
    get/2,
    patch/3,
    post/3,
    put/3
  ]).

init(_Transport, Req, Options) ->
  {upgrade, protocol, cowboy_resource}.

authorize(Type, Credentials, _Options) ->
  {ok, {<<"foo">>, <<"you-are-admin.*">>}}.

allowed(_, none) ->
  false;
allowed(Method, {_Identity, _Scope}) ->
  true.

get(Query, Options) ->
  {ok, [{<<"x">>, <<"y">>}]}.

post(Entity, Query, Options) ->
  {ok, Entity}.

put(Entity, Query, Options) ->
  {ok, <<"PUT">>}.

patch(Changes, Query, Options) ->
  {ok, <<"UPDATED">>}.

delete(Query, Options) ->
  {ok, <<"DELETED">>}.

call(<<"add">>, [X, Y], _Options) when is_number(X) ->
  {ok, X + Y};
call(_, _, _) ->
  {error, <<"NYI">>}.
```

Call:
```sh
curl -H 'Accept: application/json' localhost:8080/api/foo/1
{"x":"y"}

curl -H 'Accept: application/x-www-form-urlencoded' localhost:8080/api/foo/1
x=y

curl -H 'Accept: application/json' -H 'Content-Type: application/json' -d '{"a": "b"}' localhost:8080/api/foo
{"a":"b"}

curl -H 'Accept: application/json' -H 'Content-Type: application/x-www-form-urlencoded' -d 'a=b' localhost:8080/api/foo
{"a":"b"}

curl -H 'Accept: application/json' -H 'Content-Type: application/json' -d '{"a": "b"}' -X PUT localhost:8080/api/foo/1
"PUT"

curl -H 'Accept: application/json' -H 'Content-Type: application/json' -d '{"a": "c"}' -X PATCH localhost:8080/api/foo
"UPDATED"

curl -H 'Accept: application/json' -X DELETE localhost:8080/api/foo/1
"DELETED"

curl -H 'Content-Type: application/rpc+json' -d '[["add", [123, 321], "id999"], ["add", [true, 321], "id998"]]' localhost:8080/api/foo
[[null, 444, "id999"], ["badarg", null, "id998"]]

curl -H 'Content-Type: application/rpc+json' -d '[["nonexisting", [123, 321], "id997"]]' localhost:8080/api/foo
[["NYI", null, "id997"]]
```

cowboy_patch
--------------

Middleware for easing exposing RESTful services, helps dealing with unfriendly intermediary network environment which sometimes disallow certain critical HTTP methods and/or headers.
This one allows to tunnel information via URI and headers.
Please, consult comments in the code so far.

Should you put `cowboy_patch` in middleware chain, request will be updated automatically.

cowboy_mrouter
--------------

Middleware allowing to match HTTP method in standard router.

Inserting `cowboy_mrouter` into middleware chain before `cowboy_router` mangles incoming requests prepending URIs with HTTP method.
Router rules then can match methods directly, e.g.:
```erlang
{"/GET/foo/bar/[...]", Handler, Params}
```

cowboy_rpc
--------------

Handler for simple JSON RPC queries helping exposing level-0 REST services.

Router configuration:
```erlang
%% in env key of protocol configuration:
{"/rpc/[...]", cowboy_rpc, [{handler, {rpc, process}}]}
```

Handler can be:
- an atom `Mod` calls `Mod:handle(...)`;
- a 2-tuple of atoms: `{Mod, Fun}` calls `Mod:Fun(...)`;
- a `Pid :: pid()` calls `gen_server:call(Pid, ...)`.

In rpc.erl:
```erlang
-export([process/3]).
process(<<"add">>, X, Y) when is_number(X) and is_number(Y) ->
  {ok, X + Y}.
process(<<"add">>, _X, _Y) ->
  {error, badarg}.
```

Call:
```sh
curl -d '[["add", [123, 321], "id999"], ["add", [true, 321], "id998"]]' localhost:8080/
[[null, 444, "id999"], ["badarg", null, "id998"]]

curl -d '[["nonexisting", [123, 321], "id997"]]' localhost:8080/
[["enoent", null, "id997"]]
```

cowboy_ua
--------------

```erlang
{UserAgentHeader, Req2} = cowboy_req:headers(<<"user-agent">>, Req),
Agent = cowboy_ua:agent(UserAgentHeader),
Platform = cowboy_ua:platform(UserAgentHeader).
```

Should you put `cowboy_ua` in middleware chain, handler options will be augmented with `{useragent, {Agent, Platform}}` tuple.

cowboy_msie_fix_accept
--------------

By default MSIE [sends](https://github.com/extend/cowboy/issues/441) `Accept: application/x-ms-application, image/jpeg, application/xaml+xml, image/gif, image/pjpeg, application/x-ms-xbap, application/vnd.ms-excel, application/vnd.ms-powerpoint, application/msword, */*` header which is inappropriate to make hypermedia choice of the least surprise -- it anticipates `text/html` mime as the last fallback.
This makes creating REST services somewhat difficult.

Should you put `cowboy_msie` in middleware chain after `cowboy_router` and `cowboy_ua`, `Accept: ` will be substituted with `text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8` which seems what other browsers send by default.

cowboy_session
--------------

Manage your sessions.

Insert `cowboy_session` middleware somewhere between `cowboy_router` and `cowboy_handler`:

```erlang
  {middlewares, [
    ...
    cowboy_router,                % determine handler and its options
    ...
    cowboy_session,               % requires session_opts in environment
    ...
    cowboy_handler,               % process request
    ...
  ]}
```

Add session options to request environment:

```erlang
{env, [
    ...
    {session_opts, {SessionImplementationModuleName, ImplementationOptions}},
    ...
  ]}
```

E.g. for bundled `cowboy_cookie_session` implementation:

```erlang
{session_opts, {cowboy_cookie_session, {
    <<"sid">>,       % cookie name
    <<"tOpcekpet">>, % encryption secret
    1000,            % cookie time-to-live in seconds
    <<"/">>}}}       % cookie path
```

Access, modify or drop session:

```erlang
{Session, Req2} = cowboy_session:get(Req),
Req3 = cowboy_session:set(NewSession, Req2),
Req4 = cowboy_session:drop(Req3).
```

Consider writing other implementations :)

cowboy_common_handler
--------------

Handler for common request cases.

Router configuration:
```erlang
%% in env key of protocol configuration:
{"/common/[...]", cowboy_common_handler, [{handler, common_handler}]}
```

In common_handler.erl:
```erlang
-export([action/3]).

% render index_view template with Session passed as data there
action(<<"GET">>, [], Req) ->
  {Session, Req2} = cowboy_session:get(Req),
  {render, index_view, Session, Req2};

% status, headers, body
action(<<"GET">>, [<<"bar">>], _Req) ->
  {200, [], <<"Hello Bar!\n">>, _Req};

% action takes full care of response
action(_, _, Req) ->
  {ok, Req2} = cowboy_req:reply(200, [], <<"Hello World\n">>, Req),
  {ok, Req2};

% login/logout: redirect, new session
action(<<"POST">>, [<<"login">>], Req) ->
  {Session, Req2} = cowboy_session:get(Req),
  Session2 = case Session of
    undefined ->
      [{user, <<"DVV">>}];
    _ ->
      [{user, <<"DVV">>} | lists:keydelete(user, 1, Session)]
  end,
  Req3 = cowboy_session:set(Session2, Req2),
  {redirect, <<"/home">>, Req3};

action(<<"POST">>, [<<"logout">>], Req) ->
  Req2 = cowboy_session:drop(Req),
  {redirect, <<"/login">>, Req2}.
```

cowboy_stack
--------------

An attempt to relax cowboy middleware.

E.g. to apply `cowboy_patch:patch_accept/1` only to requests with prefix `"/api/*"`:

```erlang
  {middlewares, [
    ...
    cowboy_stack,
    ...
  ]},
  {env, [
    ...
    {stack, [
      {<<"/api/">>, cowboy_patch, patch_accept, []}
    ]},
    ...
  ]}
```

License (MIT)
-------

Copyright (c) 2013 Vladimir Dronnikov <dronnikov@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
