Stable
==============

Collection of assorted helpers for [Cowboy](https://github.com/extend/cowboy) web server.

cowboy_patch
--------------

Middleware for easing exposing RESTful services, helps dealing with unfriendly intermediary network environment which sometimes disallow certain critical HTTP methods and/or headers.
This one allows to tunnel information via URI.
Please, consult [comments in the code](https://github.com/dvv/stable/blob/master/src/cowboy_patch.erl#L9-L18) so far.

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

cowboy_cookie_session
--------------

Manage a moderately sized sessions inside secure signed encrypted cookies.

```erlang
handle(Req, State) ->

  % session params
  SessionOpts = {
      <<"s">>,    % cookie name
      <<"FOO">>,  % cipher secret
      1000,       % session time-to-live in seconds, older sessions are expired
      <<"/">>     % cookie path
    },

  % get previous session
  {Session, Req2} = cowboy_cookie_session:get_session(SessionOpts, Req),

  % do the job
  % ...
  {Status, Headers, Body, Req3} = {200, [], <<"OK">>, Req2},

  % set new session
  Req4 = case KeepSession of
      true ->
        Session2 = {foo, bar},
        cowboy_cookie_session:set_session(Session2, SessionOpts, Req3);
      false ->
        cowboy_cookie_session:set_session(undefined, SessionOpts, Req3)
    end,

  % respond
  {ok, Req5} = cowboy_req:reply(Status, Headers, Body, Req4),
  {ok, Req5, State}.
```

cowboy_cookie_session as middleware
--------------

Protocol options passed to `cowboy:start_http/4` should contain:
```erlang

  ...

  % middleware
  {middlewares, [
      cowboy_router,
      cowboy_cookie_session, % requires session_opts in env. see below
      cowboy_handler]},

  % environment
  {env, [
    % session parameters
    {session_opts, {
        <<"sid">>,       % cookie name
        <<"tOpcekpet">>, % encryption secret
        1000,            % cookie time-to-live in seconds
        <<"/">>}},       % cookie path
    ...
```

Then in the handler:

```erlang
init(_Transport, Req, Opts) ->

  % get previous session
  {session, Session, SessionOpts} = lists:keyfind(session, 1, Opts),
  {ok, Req, {Session, SessionOpts}}.

handle(Req, {Session, SessionOpts} = State) ->

  % do the job
  % ...
  {Status, Headers, Body, Req2} = {200, [], <<"OK">>, Req},

  % mangle the session
  Session2 = [{foo, bar} | Session],

  % set new session
  Req3 = cowboy_cookie_session:set_session(Session2, SessionOpts, Req2),

  % respond
  {ok, Req4} = cowboy_req:reply(Status, Headers, Body, Req3),
  {ok, Req4, State}.
```

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
-export([handler/4]).

% render
handler(<<"GET">>, [], _Req, Session) ->
  {render, index_view, Session, _Req};

% status, headers, body
handler(<<"GET">>, [<<"bar">>], _Req, Session) ->
  {200, [], <<"Hello Bar!\n">>, _Req};

% handler takes full care of response
handler(_, _, Req, _Session) ->
  {ok, Req2} = cowboy_req:reply(200, [], <<"Hello World\n">>, Req),
  {ok, Req2};

% login/logout: redirect, new session
handler(<<"POST">>, [<<"login">>], Req, Session) ->
  Session2 = case Session of
    undefined ->
      [{user, <<"DVV">>}];
    _ ->
      [{user, <<"DVV">>} | lists:keydelete(user, 1, Session)]
  end,
  % @todo needa know full path
  {redirect, <<"/common">>, Req, Session2};

handler(<<"POST">>, [<<"logout">>], Req, _Session) ->
  Session2 = undefined,
  % @todo needa know full path
  {redirect, <<"/common">>, Req, Session2}.
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
