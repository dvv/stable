%%
%% @doc Simple CSRF prevention
%%
%%      NB: Apply after cowboy_session middleware
%%

-module(cowboy_csrf).
-author('rambocoder <erlang@rambocoder.com>').

-behaviour(cowboy_middleware).
-export([execute/2]).

%%
%% @doc Middleware verifying CSRF toaken in request.
%%

execute(Req0, Env0) ->
  case cowboy_req:method(Req0) of
  	{<<"GET">>, Req1} -> {ok, Req1, Env0};
  	{<<"HEAD">>, Req1} -> {ok, Req1, Env0};
  	{<<"OPTIONS">>, Req1} -> {ok, Req1, Env0};
  	{_, Req1} ->
  	  % check if CSRF token is in body, query string, header
  	  case csrf_from_body(Req1) of
  	  	{undefined, Req2} -> 
			{ok, Req3} = cowboy_req:reply(403, [], "Invalid CSRF Token.", Req2),			
			{error, 403, Req3};
  	  	{error, _Reason} -> {error, 500, Req1};
  	  	{TokenValue, Req2} ->
  	  		{Session, Req3} = cowboy_session:get(Req2),
  	  		case proplists:get_value(csrf_token, Session, undefined) of
  	  			TokenValue -> {ok, Req3, Env0};
  	  			_ -> 
  	  				% no csrf_token in session found
  	  				% let's generate a new one and add it
  	  				NewToken = base64:encode(crypto:strong_rand_bytes(32)),
  	  				Session2 = Session ++ {csrf_token, NewToken},
  	  				cowboy_session:set(Session2, Req3),
  	  				{error, 403, Req3}
  	  		end  	  	
  	  end
  end.

csrf_from_body(Req0) ->
	% check in the body
	case cowboy_req:body_qs(Req0) of
		{error, Reason} -> {error, Reason};
		{ok, BodyQs, Req1} -> 
			case proplists:get_value(<<"_csrf">>, BodyQs, undefined) of
				undefined -> csrf_from_querystring(Req1);
				TokenValue -> {TokenValue, Req1}
			end
	end.

csrf_from_querystring(Req0) ->
	% check in the query string
	case cowboy_req:qs_val(<<"_csrf">>, Req0, undefined) of
		{undefined, Req1} -> csrf_from_header(Req1);
		{TokenValue, Req1} -> {TokenValue, Req1}
	end.

csrf_from_header(Req0) ->
	% check in the header
	case cowboy_req:header(<<"x-csrf-token">>, Req0, undefined) of
		{undefined, Req1} -> {undefined, Req1};
		{TokenValue, Req1} -> {TokenValue, Req1}
	end.