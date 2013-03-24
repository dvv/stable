-module(cowboy_patch).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2]).

-export([
    patch_headers/1,
    patch_method/1,
    patch_pragmatic_rest/1
  ]).

%%
%% @doc Analyze querystring tokens: those starting with "_x-" augment request
%%      headers and are pruned from querystring.
%%
%% @doc E.g. ?_x-Accept=application%2fjson&foo=bar effectively results in
%%      ?foo=bar plus [{<<"accept">>, <<"application/json">>}] in headers.
%%
%% @doc Can help expose services in more RESTful way in unfriendly environment
%%      when network intermediary may rip critical headers.
%%
-spec patch_headers(cowboy_req:req()) -> cowboy_req:req().

patch_headers(Req) ->
  {Qs, Req2} = cowboy_req:qs_vals(Req),
  {Headers, Req3} = cowboy_req:headers(Req2),
  {Headers2, Qs2} = lists:foldl(fun (Token = {Name, Value}, {Heads, Vars}) ->
    case Name of
      <<$_, $x, $-, Rest/binary>> ->
        {[{Rest, Value} | Heads], Vars};
      _Else ->
        {Heads, [Token | Vars]}
    end
  end, {Headers, []}, Qs),
  cowboy_req:set([
      {headers, Headers2},
      {qs_vals, Qs2}
    ], Req3).

%%
%% @doc Reset request method in accordance to well-known method-override
%%      headers.
%%
%% http://fandry.blogspot.ru/2012/03/x-http-header-method-override-and-rest.html
%%
%% @doc Can help expose services in more RESTful way in unfriendly environment
%%      when network intermediary may disallow certain methods (PUT/DELETE).
%%
-spec patch_method(cowboy_req:req()) -> cowboy_req:req().

patch_method(Req) ->
  [Method, Headers] = cowboy_req:get([method, headers], Req),
  {Method2, Headers2} = case method(Headers, [
      <<"x-http-method">>,
      <<"x-http-method-override">>,
      <<"x-method-override">>
    ]) of
    false ->
      {Method, Headers};
    {NewMethod, HeaderName} ->
      {NewMethod, lists:keydelete(HeaderName, 1, Headers)}
  end,
  cowboy_req:set([
      {method, cowboy_bstr:to_upper(Method2)},
      {headers, Headers2}
    ], Req).

method(_, []) ->
  false;
method(Headers, [Override | Tail]) ->
  case lists:keyfind(Override, 1, Headers) of
    {_, Method} -> {Method, Override};
    false -> method(Headers, Tail)
  end.

%%
%% @doc Patch request in accordance to apigen recommendations.
%%
%% http://coolthingoftheday.blogspot.com.es/2012/03/another-free-from-team-at-apigee-api.html
%%
-spec patch_pragmatic_rest(cowboy_req:req()) -> cowboy_req:req().

patch_pragmatic_rest(Req) ->
  [Headers, Path, Method] = cowboy_req:get([headers, path, method], Req),
  % honor URI suffix (overrides Accept: if any)
  % @todo: mimetypes is gen_server-based -- may be bottleneck
  Req2 = case mimetypes:filename(Path) of
    [<<"application/octet-stream">>] ->
      Req;
    NewAccepts -> % join using comma
      cowboy_req:set([
          {headers, [{<<"accept">>, binary_join(NewAccepts, <<$,>>)} |
              lists:keydelete(<<"accept">>, 1, Headers)]},
          {path, filename:rootname(Path)}
        ], Req)
  end,
  % extract standard API parameters
  {Qs, Req3} = cowboy_req:qs_vals(Req2),
  % @todo rewrite in favor of lists:foldl
  {Meta, Qs2} = lists:partition(fun is_meta_param/1, Qs),
  Req4 = cowboy_req:set([{qs_vals, Qs2}], Req3),
  % override method for POST requests
  {Req5, Meta2} = case lists:keytake(<<"method">>, 1, Meta) of
    {value, {<<"method">>, NewMethod}, Meta1} when Method =:= <<"POST">> ->
      {cowboy_req:set([
          {method, cowboy_bstr:to_upper(NewMethod)}
        ], Req4), Meta1};
    _ ->
      {Req4, Meta}
  end,
  % @todo Range: items=... header should parse to limit/offset
  % store standard API parameters in request meta
  % @todo make the keys atoms
  cowboy_req:set_meta(meta, Meta2, Req5).

is_meta_param({<<"fields">>, _}) -> true;
is_meta_param({<<"limit">>, _}) -> true;
is_meta_param({<<"offset">>, _}) -> true;
is_meta_param({<<"method">>, _}) -> true;
is_meta_param({<<"suppress_response_codes">>, _}) -> true;
is_meta_param({_, _}) -> false.

binary_join([H], _Sep) ->
  << H/binary >>;
binary_join([H|T], Sep) ->
  << H/binary, Sep/binary, (binary_join(T, Sep))/binary >>;
binary_join([], _Sep) ->
  <<>>.

%%
%% @doc Middleware applying patch_headers/1 and patch_method/1.
%%

execute(Req, Env) ->
  {ok, patch_method(patch_headers(Req)), Env}.
