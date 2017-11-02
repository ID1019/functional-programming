%% The two states of the chopstick.

available() ->
    receive 
	%% The first solution.
	{request, From} ->
	    From ! granted,
	    gone(na);


	%% Keeping track of requests.
	{request, Ref, From} ->
	    From ! {granted, Ref},
	    gone(Ref);


	quit ->
	    ok
    end.

gone(Ref) ->
    receive 
	%% The first solution
	return ->
	    available();

	%% Keeping track of request
	{return, Ref} ->
	    available();

	quit ->
	    ok
    end.
