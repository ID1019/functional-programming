-module(chopstick).

-export([start/0, request/1, request/2, request/3, return/1, return/2, asynch/2,  wait/2, quit/1]).

start() ->
    Stick = spawn_link(fun() -> init() end),
    {stick, Stick}.






%% The synchronous version of requesting a chopstick.

request({stick, Pid}) ->
    Pid ! {request, self()},
    receive 
	granted ->
	    ok
    end.

return({stick, Pid}) ->
    Pid ! return.






%% Using a timeout to detect deeadlock , does it work?

request({stick, Pid}, After) ->
    Pid ! {request, self()},
    receive 
	granted ->
	    ok
    after After ->
	    no
    end.









%% The better version, we keep track of requests.

request({stick, Pid}, Ref, After) ->
    Pid ! {request, Ref, self()},
    wait(Ref, After).

wait(Ref, After) ->
    receive 
	{granted, Ref} ->
	    ok;
	{granted, _} ->
	    wait(Ref, After)
    after After ->
	    no
    end.

return({stick, Pid}, Ref) ->
    Pid ! {return, Ref}.

%% A asynchronous request, divided into sending the request and
%% waiting for the reply.

asynch({stick, Pid}, Ref) ->
    Pid ! {request, Ref, self()}.


%% To terminate the process.

quit({stick, Pid}) ->
    Pid ! quit.


%% Initalizing the chopstick.

init() ->
    available().

%% The two states of the chopstick.

available() ->
    receive 
	%% The first solution.
	{request,  From} ->
	    From ! granted,
	    gone();

	quit ->
	    ok
    end.

gone() ->
    receive 
	%% The first solution
        return ->
	   available();

	quit ->
	    ok
    end.


		   
