%%
%% @doc This module handles simplified JSON RPC batch requests.
%%
%% Format is basically JSON-RPC (http://www.jsonrpc.org/specification)
%%   with the following changes:
%%
%% - requests are triplet arrays: [Method, Params, Id].
%%
%% - responses are as well triplet arrays or null:
%%     null | [null, Result, Id] | [Error, null, Id].
%%
%% Example:
%%
%% curl -i -d "[['add', [2, 3], 101], ['add1', [2, 3], 102]]" localhost:8080
%% 200 OK [[null, 5, 101], ["enoent", null, 102]
%%
%% curl -i -d "[['add', ['2', 3], 103], ['add', [2, 3], null]]" localhost:8080
%% 200 OK [["einval", null, 103], null]
%%
%% curl -i -d "[['add', [2, 3]," localhost:8080
%% 400 Bad request einval
%%
%% N.B.: JSON is handled by [jsx](https://github.com/talentdeficit/jsx)
%%
%% @doc Handler functions must return {ok, Result} or {error, Reason}
%%

-module(cowboy_rpc).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_http_handler).
-export([init/3, terminate/3, handle/2]).

init(_Transport, Req, Opts) ->
  % only POST is allowed
  {Method, Req2} = cowboy_req:method(Req),
  case Method of
    <<"POST">> ->
      {ok, Req2, Opts};
    _Else ->
      {ok, Req3} = cowboy_req:reply(405, [
          {<<"allow">>, <<"POST">>}
        ], <<>>, Req2),
      {shutdown, Req3, undefined}
  end.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, Opts) ->
  % get request JSON
  % @todo support streaming?
  {ok, JSON, Req2} = cowboy_req:body(Req),
  % parse options
  {handler, Handler} = lists:keyfind(handler, 1, Opts),
  % decode JSON to batch request and process the latter
  {ok, Req3} = try jsx:decode(JSON, [single_quoted_strings]) of
    % need more data?
    {incomplete, _Fun} ->
      % report invalid JSON
      % @todo should be gone once body streaming is done
      cowboy_req:reply(400, [], <<"einval">>, Req2);
    % JSON is valid and batch request looks valid?
    % @todo: [{...}, {...}] should be also invalid
    Batch ->
      % yes -> process the batch
      Result = [try rpc(Method, Params, Id, Handler) of
        Any -> Any
      catch
        _:function_clause ->
          [<<"enoent">>, null, Id];
        _:badarg ->
          [<<"einval">>, null, Id];
        _:badarith ->
          [<<"einval">>, null, Id];
        % @todo this is defensive programming
        _:Exception ->
          [atom_to_binary(Exception, latin1), null, Id]
      end || [Method, Params, Id] <- Batch],
      % encode result back to JSON
      try jsx:encode(Result) of
        Output ->
          % respond
          cowboy_req:reply(200, [
            {<<"content-type">>, <<"application/json; charset=UTF-8">>}
          ], Output, Req2)
      catch _:_ ->
        % result can't be encoded to JSON -> report predefined error
        cowboy_req:reply(500, [], <<"einval">>, Req2)
      end
  catch _:_ ->
    % report invalid JSON
    % @todo should be not exception once jsx is made report errors
    cowboy_req:reply(400, [], <<"einval">>, Req2)
  end,
  {ok, Req3, undefined}.

%%
%% -----------------------------------------------------------------------------
%% Internal RPC helpers
%% -----------------------------------------------------------------------------
%%

-type jsonable() :: null | boolean() | list() | binary() | number().
-type rpc_result() :: list({null, jsonable(), jsonable()}).
-type rpc_error() :: list({jsonable(), null, jsonable()}).

-spec rpc(
    Method :: binary(),
    Params :: list(jsonable()),
    Id :: jsonable(),
    Handler :: atom() | {Mod :: atom(), Fun :: atom()} | pid()
  ) -> null | rpc_result() | rpc_error().

rpc(Method, Params, Id, Handler) when is_atom(Handler) ->
  reply(apply(Handler, handle, [Method | Params]), Id);

rpc(Method, Params, Id, {Mod, Fun}) ->
  reply(apply(Mod, Fun, [Method | Params]), Id);

rpc(Method, Params, null, Handler) when is_pid(Handler) ->
  reply(gen_server:cast(Handler, {Method, Params}), null);

rpc(Method, Params, Id, Handler) when is_pid(Handler) ->
  reply(gen_server:call(Handler, {Method, Params}), Id).

reply(_R, null) ->
  null;

reply(R, Id) ->
  case R of
    {ok, Result} ->
      [null, Result, Id];
    {error, Reason} ->
      % @todo Reason to binary conversion
      [Reason, null, Id]
  end.
