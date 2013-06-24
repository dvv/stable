-module(cowboy_patch).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2]).

-export([
    parse_fields/1,
    parse_sort/1,
    patch_accept/1,
    patch_headers/1,
    patch_method/1,
    patch_method_by_header/1
  ]).

execute(Req, Env) ->
  Req2 =
    parse_sort(
      parse_fields(
        patch_method(
          patch_method_by_header(
            patch_headers(Req))))),
  {Path, Req3} = cowboy_req:path(Req2),
  case Path of
    << "/api/", _/binary >> ->
      patch_accept(Req3);
    _ ->
      Req3
  end.

%%
%% @doc Transform URI suffix to Accept: header and trims the suffix.
%%
%% @doc E.g. GET /foo.json --> GET /foo Accept: application/json
%%
-spec patch_accept(cowboy_req:req()) -> cowboy_req:req().
patch_accept(Req) ->
  [Headers, Path] = cowboy_req:get([headers, path], Req),
  case mimetypes:filename(Path) of
    <<"application/octet-stream">> ->
      Req;
    [<<"application/octet-stream">>] ->
      Req;
    NewMime ->
      cowboy_req:set([
          {headers, [{<<"accept">>, binary_join_comma(NewMime)} |
              lists:keydelete(<<"accept">>, 1, Headers)]},
          {path, filename:rootname(Path)}
        ], Req)
  end.

%%
%% @doc Consumes ?sort parameter into a list of tuples {FIELD, asc | desc}
%%      and sets sort meta.
%%
-spec parse_sort(cowboy_req:req()) -> cowboy_req:req().
parse_sort(Req) ->
  case consume_qs(<<"sort">>, Req) of
    {undefined, Req2} ->
      Req2;
    {Bin, Req2} ->
      List = lists:foldr(fun
        (<< $-, F/binary >>, A) -> [{F, desc} | A];
        (<< $+, F/binary >>, A) -> [{F, asc} | A];
        (F, A) -> [{F, asc} | A]
      end, [], binary:split(Bin, << $, >>, [global, trim])),
      cowboy_req:set_meta(sort, List, Req2)
  end.

%%
%% @doc Consumes ?fields parameter into a list of binaries and sets fields meta.
%%
-spec parse_fields(cowboy_req:req()) -> cowboy_req:req().
parse_fields(Req) ->
  case consume_qs(<<"fields">>, Req) of
    {undefined, Req2} ->
      Req2;
    {Bin, Req2} ->
      List = binary:split(Bin, << $, >>, [global, trim]),
      cowboy_req:set_meta(fields, List, Req2)
  end.

%%
%% @doc Reset request method in accordance to ?method querystring parameter.
%%
%% @doc Can help expose services in more RESTful way in unfriendly environment
%%      when network intermediary may disallow certain methods (PUT/DELETE).
%%
-spec patch_method(cowboy_req:req()) -> cowboy_req:req().
patch_method(Req) ->
  [Method] = cowboy_req:get([method], Req),
  case consume_qs(<<"method">>, Req) of
    % NB: only for POST original method
    {Bin, Req2} when Bin =/= undefined andalso Method =:= <<"POST">> ->
      cowboy_req:set([{method, cowboy_bstr:to_upper(Bin)}], Req2);
    {_, Req2} ->
      Req2
  end.

%%
%% @doc Reset request method in accordance to well-known method-override
%%      headers.
%%
%% http://fandry.blogspot.ru/2012/03/x-http-header-method-override-and-rest.html
%%
%% @doc Can help expose services in more RESTful way in unfriendly environment
%%      when network intermediary may disallow certain methods (PUT/DELETE).
%%
-spec patch_method_by_header(cowboy_req:req()) -> cowboy_req:req().
patch_method_by_header(Req) ->
  [Method, Headers] = cowboy_req:get([method, headers], Req),
  case lists:keytake(<<"x-method-override">>, 1, Headers) of
    % NB: only for POST original method
    {value, {_, Method2}, Headers2} when Method =:= <<"POST">> ->
      cowboy_req:set([
          {method, cowboy_bstr:to_upper(Method2)},
          {headers, Headers2}
        ], Req);
    _ ->
      Req
  end.

% patch_method_with_header000(Req) ->
%   [Method, Headers] = cowboy_req:get([method, headers], Req),
%   {Method2, Headers2} = lists:foldl(fun
%       ({<<"x-http-method">>, V}, {_, H}) -> {V, H};
%       ({<<"x-http-method-override">>, V}, {_, H}) -> {V, H};
%       ({<<"x-method-override">>, V}, {_, H}) -> {V, H};
%       (Other, {M, H}) -> {M, [Other | H]}
%     end, {Method, []}, Headers),
%   cowboy_req:set([
%       {method, cowboy_bstr:to_upper(Method2)},
%       {headers, Headers2}
%     ], Req).

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
  {Headers2, Qs2} = lists:foldl(fun
    ({<< $_, $x, $-, Head/binary >>, Value}, {Heads, Vars}) ->
      {[{Head, Value} | Heads], Vars};
    (Other, {Heads, Vars}) ->
      {Heads, [Other | Vars]}
  end, {Headers, []}, Qs),
  cowboy_req:set([
      {headers, Headers2},
      {qs_vals, Qs2}
    ], Req3).

% patch_range(Req, Env) ->
%   {Offset, Req2} = consume_qs(<<"offset">>, Req, 0),
%   {Limit, Req3} = consume_qs(<<"limit">>, Req2, infinity),
%   case {Offset, Limit} of
%     {0, infinity} ->
%       {ok, Req3, Env};
%     {0, Bin} ->
%       {ok, Req3, Env}
%   end.

% num(B) -> list_to_integer(binary_to_list(B)).

consume_qs(Key, Req) ->
  consume_qs(Key, Req, undefined).
consume_qs(Key, Req, Default) ->
  {Qs, Req2} = cowboy_req:qs_vals(Req),
  case lists:keytake(Key, 1, Qs) of
    {value, {Key, Bin}, Qs2} ->
      {Bin, cowboy_req:set([{qs_vals, Qs2}], Req2)};
    _ ->
      {Default, Req2}
  end.

binary_join_comma([]) ->
  <<>>;
binary_join_comma(B) when is_binary(B) ->
  B;
binary_join_comma(L) when is_list(L) ->
  << $,, R/binary >> = << << $,, (binary_join_comma(E))/binary >> || E <- L >>,
  R.
