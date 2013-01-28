%%
%% @doc Delegates actual processing to a helper module
%%      which exposes `handler(Method, Path, Req, Session)`.
%%
%% @doc Return values are matched against the common cases:
%%      - status, headers and body known
%%      - render a module to HTML with some data
%%      - response already answered
%%

-module(cowboy_common_handler).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([init/3, terminate/3, handle/2]).

init(_Transport, Req, Opts) ->
  {ok, Req, Opts}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, Opts) ->
  % get handler module
  {handler, Handler} = lists:keyfind(handler, 1, Opts),
  % get request method and path
  [Method, Path] = cowboy_req:get([method, path_info], Req),
  % get session
  %{session, Session} = lists:keyfind(session, 1, Opts),
  Session = proplists:get_value(session, Opts, undefined),
  % call handler
  case Handler:handler(Method, Path, Req, Session) of
    % we got everything for response
    {Status, Headers, Body, Req2} when is_integer(Status) ->
      {ok, Req3} = cowboy_req:reply(Status, Headers, Body, Req2),
      {ok, Req3, undefined};
    % handler already responded
    {ok, Req2} ->
      {ok, Req2, undefined};
    % template module and variables for interpolation
    {render, TemplateModule, Data, Req2} ->
      try TemplateModule:render(Data) of
        {ok, IoList} ->
          {ok, Req3} = cowboy_req:reply(
              200,
              [{<<"content-type">>, <<"text/html; charset=UTF-8">>}],
              IoList,
              Req2
            ),
          {ok, Req3, undefined};
        {error, _Error} ->
          {ok, Req3} = cowboy_req:reply(
              500,
              [],
              <<"Render error">>,
              Req2
            ),
          {ok, Req3, undefined}
      catch _:_ ->
        {ok, Req3} = cowboy_req:reply(
            500,
            [],
            <<"Render error">>,
            Req2
          ),
        {ok, Req3, undefined}
      end;
    % anything else is passed verbatim
    Else ->
      Else
  end.
