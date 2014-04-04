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
			{ok, Req3} = cowboy_req:reply(403, [], <<"Body does not contain CSRF token.">>, Req2),			
			{error, 403, Req3};
  	  	{error, _Reason} -> {error, 500, Req1};
  	  	{CSRFTokenValue, Req2} -> check_session_first(CSRFTokenValue, Req2, Env0)
  	  end
  end.

check_session_first(CSRFTokenValue, Req2, Env0) ->
	case cowboy_session:get(Req2) of
	  	{undefined, Req3} -> 
	  		Req4 = cowboy_session:set([], Req3),
			found_session(CSRFTokenValue, [], Req4, Env0);
		{Session, Req3} -> 
			found_session(CSRFTokenValue, Session, Req3, Env0)
	end.

found_session(CSRFTokenValue, Session, Req3, Env0)	->
	case proplists:get_value(csrf_token, Session, undefined) of
		undefined -> 
			% no csrf_token in session found
			% let's generate a new one and add it
			NewToken = base64:encode(crypto:strong_rand_bytes(32)),
			Session2 = Session ++ {csrf_token, NewToken},
			cowboy_session:set(Session2, Req3),
			Req4 = cowboy_req:set_resp_body(<<"Session was invalid. It did not contain CSRF token.">>, Req3),
			{error, 403, Req4};
		CSRFTokenValue ->
			{ok, Req3, Env0};
		SessionCSRFTokenValue -> 
			Reason = io_lib:format(<<"Session CSRF token: ~p did not match body CSRF token: ~p">>,[SessionCSRFTokenValue, CSRFTokenValue]),
			Req4 = cowboy_req:set_resp_body(Reason, Req3),
			{error, 403, Req4}
	end.


csrf_from_body(Req0) ->
	% check in the body
	case cowboy_req:body(Req0) of
		{error, Reason} -> {error, Reason};
		{ok, Buffer, Req1} ->
			BodyQs = cowboy_http:x_www_form_urlencoded(Buffer),
			Req2   = cowboy_req:set([{buffer, Buffer}], Req1),
			Req3   = cowboy_req:set([{body_state, waiting}], Req2),
			case proplists:get_value(<<"_csrf">>, BodyQs, undefined) of
				undefined -> csrf_from_querystring(Req3);
				TokenValue -> {TokenValue, Req3}
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
