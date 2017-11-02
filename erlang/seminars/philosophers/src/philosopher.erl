-module(philosopher).

-export([start/7]).

-define(Dream, 1000).
-define(Eat, 50).

-define(Delay, 200).

-define(Timeout, 1000).


start(Hungry, Strength, Left, Right, Name, Ctrl, Seed) ->
    spawn_link(fun() -> init(Hungry, Strength, Left, Right, Name, Ctrl, Seed) end).

init(Hungry, Strength, Left, Right, Name, Ctrl, Seed) ->
    Gui = gui:start(Name),
    rand:seed(exsplus, {Seed, Seed, Seed}),
    dreaming(Hungry, Strength, Left, Right, Name, Ctrl, Gui).

dreaming(0, Strength, _Left, _Right, Name, Ctrl, Gui) ->
    io:format("~s happy, strength is still ~w~n", [Name, Strength]),
    Gui ! stop,
    Ctrl ! done;
dreaming(Hungry, 0, _Left, _Right, Name, Ctrl, Gui) ->
    io:format("~s starved to death, hungry is down to ~w~n", [Name, Hungry]),
    Gui ! stop,
    Ctrl ! done;
dreaming(Hungry, Strength, Left, Right, Name, Ctrl, Gui) ->
    io:format("~s dreaming~n",[Name]),
    delay(?Dream),
    waiting(Hungry, Strength, Left, Right, Name, Ctrl, Gui).


waiting(Hungry, Strength, Left, Right, Name, Ctrl, Gui) ->
    Gui ! waiting,
    io:format("~s waiting - ~w to go~n",[Name, Hungry]),

    case chopstick:request(Left) of
	ok ->
	    delay(?Delay),
	    case chopstick:request(Right) of 
		ok ->
		    io:format("~s received both sticks~n",[Name]),
		    eating(Hungry, Strength, Left, Right, Name, Ctrl, Gui)
	    end
    end.	    


eating(Hungry, Strength, Left, Right, Name, Ctrl, Gui) ->
    Gui ! enter,
    io:format("~s eating~n",[Name]),

    delay(?Eat),    

    chopstick:return(Left),
    chopstick:return(Right),

    Gui ! leave,
    dreaming(Hungry-1, Strength, Left, Right, Name, Ctrl, Gui).


delay(T) ->
    sleep(T).

sleep(0) -> ok; 
sleep(T) -> timer:sleep(rand:uniform(T)).

		       
