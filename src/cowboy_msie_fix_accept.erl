%%
%% @doc Modify default Accept: header coming from MSIE.
%%
%%      Should be fixing https://github.com/extend/cowboy/issues/441
%%
%%      NB: Apply after cowboy_router and cowboy_ua middlewares.
%%

-module(cowboy_msie_fix_accept).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
  {handler_opts, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
  case lists:keyfind(useragent, 1, HandlerOpts) of
    {useragent, {ie, windows}} ->
      {Headers, Req2} = cowboy_req:headers(Req),
      Req3 = cowboy_req:set([{headers, [
          {<<"accept">>,
          <<"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>}
            | Headers]}], Req2),
      {ok, Req3, Env};
    _Else ->
      {ok, Req, Env}
  end.
