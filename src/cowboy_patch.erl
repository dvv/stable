-module(cowboy_patch).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2]).

-export([patch_headers/1, patch_method/1]).

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
%% @doc Middleware applying patch_headers/1 and patch_method/1.
%%

execute(Req, Env) ->
  {ok, patch_method(patch_headers(Req)), Env}.
