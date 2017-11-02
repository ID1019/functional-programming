
waiting(Hungry, Strength, Left, Right, Name, Ctrl, Gui) ->
    Gui ! waiting,
    io:format("~s waiting - ~w to go~n",[Name, Hungry]),

    Ref = make_ref(),
    
    chopstick:asynch(Left, Ref),
    delay(?Delay),
    chopstick:asynch(Right, Ref),

    case chopstick:wait(Ref, ?Timeout) of
	ok ->
	    io:format("~s received stick~n",[Name]),

	    case chopstick:wait(Ref, ?Timeout) of
		ok ->
		    io:format("~s received stick~n",[Name]),
		    eating(Hungry, Strength, Left, Right, Name, Ctrl, Gui);
	        no ->
		    io:format("~s giving up~n",[Name]),
		    chopstick:return(Left),
		    chopstick:return(Right),
		    Gui ! abort,
		    dreaming(Hungry, Strength-1, Left, Right, Name, Ctrl, Gui)
	    end;
	no ->
	    io:format("~s giving up~n",[Name]),
	    chopstick:return(Left),
	    chopstick:return(Right),
	    Gui ! abort,
	    dreaming(Hungry, Strength-1, Left, Right, Name, Ctrl, Gui)
    end.
