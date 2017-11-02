-module(dinner).

-export([start/1, stop/0]).

start(N) ->
    Seed = 1234,
    Dinner = spawn(fun() -> init(N, Seed) end),
    register(dinner, Dinner).

stop() ->
    dinner ! abort.

init(N, Seed) ->
    C1 = chopstick:start(),    
    C2 = chopstick:start(),
    C3 = chopstick:start(),
    C4 = chopstick:start(),
    C5 = chopstick:start(),
    Ctrl = self(),
    philosopher:start(N, 5, C1, C2, "Arendt", Ctrl, Seed+1),
    philosopher:start(N, 5, C2, C3, "Hypatia", Ctrl, Seed+2),
    philosopher:start(N, 5, C3, C4, "Simone", Ctrl, Seed+3),
    philosopher:start(N, 5, C4, C5, "Elisabeth", Ctrl, Seed+4),
    philosopher:start(N, 5, C1, C5, "Ayn", Ctrl, Seed+5),
    wait(5,[C1,C2,C3,C4,C5]).

wait(0, Chopsticks) ->
    lists:foreach(fun(C) -> chopstick:quit(C) end, Chopsticks);
wait(N, Chopsticks) ->
    receive
	done ->
	    wait(N-1, Chopsticks);
	abort ->
	    erlang:exit(abort)
    end.
    



    
    
    
    
		  
