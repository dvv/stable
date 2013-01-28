%%
%% @doc Simple user-agent determination
%%

-module(cowboy_ua).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2]).

-export([agent/1, platform/1]).

-spec agent(binary()) -> undefined | atom().

agent(Header) ->
  lookup(cowboy_bstr:to_lower(Header), [
      {<<"msie">>,      ie},
      {<<"chrome">>,    chrome},
      {<<"firefox">>,   firefox},
      {<<"opera">>,     opera},
      {<<"safari">>,    safari},
      {<<"konqueror">>, konqueror},
      {<<"curl">>,      curl}
    ]).

-spec platform(binary()) -> undefined | atom().

platform(Header) ->
  lookup(cowboy_bstr:to_lower(Header), [
      {<<"windows">>,   windows},
      {<<"linux">>,     linux},
      {<<"os x ">>,     osx},
      {<<"android">>,   android},
      {<<"ipod">>,      ipod},
      {<<"iphone">>,    iphone},
      {<<"ipad">>,      ipad}
    ]).

lookup(_Header, []) ->
  undefined;
lookup(Header, [{Pattern, Result} | Rest]) ->
  case binary:match(Header, Pattern) of
    nomatch -> lookup(Header, Rest);
    _ -> Result
  end.

%%
%% @doc Middleware pushing user-agent info in request handler options.
%%

execute(Req, Env) ->
  {UserAgentHeader, Req2} = cowboy_req:header(<<"user-agent">>, Req, <<>>),
  {handler_opts, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
  HandlerOpts2 = [{useragent, {
      agent(UserAgentHeader),
      platform(UserAgentHeader)
    }} | HandlerOpts],
  Env2 = [{handler_opts, HandlerOpts2} | lists:keydelete(handler_opts, 1, Env)],
  {ok, Req2, Env2}.
