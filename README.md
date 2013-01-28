Stable
==============

Library of assorted helpers for [Cowboy](https://github.com/extend/cowboy) web server.

cowboy_patch
--------------

Middleware for easing exposing RESTful services, helps dealing with unfriendly intermediary network environment which sometimes disallow certain critical HTTP methods and/or headers.
This one allows to tunnel information via URI.
Please, consult [comments in the code](src/cowboy_patch.erl#L9-18) so far. 

cowboy_rpc
--------------

Handler for simple JSON RPC queries helping exposing level-0 REST services.

Router configuration:
```erlang
%% in env key of protocol configuration:
{"/rpc/[...]", cowboy_rpc, [{handler, {rpc, process}}]}
```

In rpc.erl:
```erlang
-export([process/3]).
process(<<"add">>, X, Y) ->
  {ok, X + Y}.
```

Call:
```sh
curl -d '[["add", [123, 321], 999]]' localhost:8080/
[[null, 444, 999]]

curl -d '[["nonexisting", [123, 321], 999]]' localhost:8080/
[["enoent", null, 999]]
```

[License](LICENSE.txt)
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
