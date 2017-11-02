-module(test).


-compile(export_all).


gray() ->
    S0 = erlang:system_time(milli_seconds),
    Image = ppm:read("morfar.ppm"),
    S1 = erlang:system_time(milli_seconds),
    Gray = image:gray(Image),
    S2 = erlang:system_time(milli_seconds),
    ppm:write("gray.ppm", Gray),
    io:format("reading in  ~w ms ~n", [S1 - S0]),
    io:format("gray in  ~w ms ~n", [S2 - S1]),
    io:format("total in  ~w ms ~n", [(S2 - S0)]).    


solar() ->
    S0 = erlang:system_time(milli_seconds),
    Image = ppm:read("morfar.ppm"),
    S1 = erlang:system_time(milli_seconds),
    Gray = image:gray(Image),
    S2 = erlang:system_time(milli_seconds),
    Solar = image:solar(Gray),
    S3 = erlang:system_time(milli_seconds),
    ppm:write("solar.ppm", Solar),
    io:format("reading in  ~w ms ~n", [S1 - S0]),
    io:format("gray in  ~w ms ~n", [S2 - S1]),
    io:format("solar in  ~w ms ~n", [S3 - S2]),
    io:format("total in  ~w ms ~n", [(S3 - S0)]).    



seq() ->
    S0 = erlang:system_time(milli_seconds),
    Image = ppm:read("morfar.ppm"),
    S1 = erlang:system_time(milli_seconds),
    Gray = image:gray(Image),
    S2 = erlang:system_time(milli_seconds),
    Red = image:reduce(10, Gray),
    S3 = erlang:system_time(milli_seconds),
    ppm:write("test.ppm", Red),
    S4 = erlang:system_time(milli_seconds),
    io:format("reading in  ~w ms ~n", [S1 - S0]),

    io:format("gray in  ~w ms ~n", [S2 - S1]),
    io:format("reduce in  ~w ms ~n", [S3 - S2]),
    io:format("writing in  ~w ms ~n", [S4 - S3]),
    io:format("total in  ~w ms ~n", [(S4 - S0)]).


bench(Schedulers) ->
    lists:foreach(fun(S) ->
			  io:format("~n nr of schedulers ------ ~w~n", [S]),
			  erlang:system_flag(schedulers_online, S),
			  test(0),
			  test(1),    
			  test(2),
			  test(3),
			  test(4)
		  end, Schedulers).

test(0) ->
    S = erlang:system_time(milli_seconds),
    Self = self(),
    W = nuller(Self),
    ppm:reader("morfar.ppm", W),
    receive 
	done ->
	    ok
    end,
    T = erlang:system_time(milli_seconds) - S,
    io:format("reading only in ~w ms ~n", [T]);
    

test(1) ->
    S = erlang:system_time(milli_seconds),
    Self = self(),
    W = nuller(Self),
    G = image:grayer(W),
    ppm:reader("morfar.ppm", G),
    receive 
	done ->
	    ok
    end,
    T = erlang:system_time(milli_seconds) - S,
    io:format("reading and turning gray in ~w ms ~n", [T]);


test(2) ->
    S = erlang:system_time(milli_seconds),
    Self = self(),
    W = ppm:writer("gray.ppm", Self),
    B = image:blurer(W),
    G = image:grayer(B),
    ppm:reader("morfar.ppm", G),
    receive 
	done ->
	    ok
    end,
    T = erlang:system_time(milli_seconds) - S,
    io:format("reading turning gray and writing ~w ms ~n", [T]);


test(3) ->
    S = erlang:system_time(milli_seconds),
    Self = self(),
    W = ppm:writer("blur.ppm", Self),
    B = image:blurer(W),
    G = image:grayer(B),
    ppm:reader("morfar.ppm", G),
    receive 
	done ->
	    ok
    end,
    T = erlang:system_time(milli_seconds) - S,
    io:format("reading turning gray, blur and writing ~w ms ~n", [T]);

test(4) ->
    S = erlang:system_time(milli_seconds),
    Self = self(),
    W = nuller(Self),
    B = image:blurer(W),
    G = image:grayer(B),
    ppm:reader("morfar.ppm", G),
    receive 
	done ->
	    ok
    end,
    T = erlang:system_time(milli_seconds) - S,
    io:format("reading turning gray, blur no writing ~w ms ~n", [T]);

test(5) ->
    S = erlang:system_time(milli_seconds),
    Self = self(),
    W = ppm:writer("smallsolar.ppm", Self),
    B = image:solarer(W),
    G = image:grayer(B),
    ppm:reader("small.ppm", G),
    receive 
	done ->
	    ok
    end,
    T = erlang:system_time(milli_seconds) - S,
    io:format("reading turning gray, solar and writing ~w ms ~n", [T]);

test(6) ->
    S = erlang:system_time(milli_seconds),
    Self = self(),
    W = ppm:writer("reduced.ppm", Self),
    B = image:reducer(10, W),
    G = image:grayer(B),
    ppm:reader("morfar.ppm", G),
    receive 
	done ->
	    ok
    end,
    T = erlang:system_time(milli_seconds) -  S,
    io:format("reading turning gray, reducing and writing ~w ms ~n", [T]);

test(7) ->
    S = erlang:system_time(milli_seconds),
    Self = self(),
    W = ppm:writer("solar.ppm", Self),
    B = image:solarer(W),
    G = image:grayer(B),
    ppm:reader("morfar.ppm", G),
    receive 
	done ->
	    ok
    end,
    T = erlang:system_time(milli_seconds) -  S,
    io:format("reading turning gray, reducing and writing ~w ms ~n", [T]).


    



	    
nuller(Out) ->    
    spawn_link(fun() -> null_init(Out) end).

null_init(Out) ->
    receive
	{header, _} ->
	    null(Out);
	done ->
	    Out ! done
    end.
	    
null(Out) ->	
    receive 
	{line, _, _} ->
	    null(Out);
	done ->
	    Out ! done
    end.

		  

demo(1) ->
    W = spawn_link(fun() -> writer() end),
    B = image:blurer(W),
    G = image:grayer(B),
    spawn_link(fun() -> reader(G) end);

demo(2) ->
    W = spawn_link(fun() -> writer() end),
    B = image:solarer(W),
    G = image:grayer(B),
    spawn_link(fun() -> reader(G) end).



reader(Out) ->
    Out ! {header, {rgb, {3,3}, 255}},
    Out ! {line, 1, [{1,1,1}, {2,2,2},{3,3,3}]},
    Out ! {line, 2, [{4,4,4}, {0,0,0},{6,6,6}]},
    Out ! {line, 3, [{7,7,7}, {8,8,8},{9,9,9}]},
    Out ! done.



writer() ->
    receive
	{header, {P, Size, D}} ->
	    io:format("header ~w ~w ~w~n", [P, Size, D]),
	    writer_lines();
	done ->
	    io:format("done~n", [])
    end.

writer_lines() ->
    receive
	{line, N, Line} ->
	    io:format("~w: ~w~n", [N, Line]),
	    writer_lines();	    
	done ->
	    io:format("done~n", [])
    end.
