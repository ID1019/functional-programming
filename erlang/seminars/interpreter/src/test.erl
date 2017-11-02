-module(test).

-compile(export_all).

test(N) ->
    %% The default is the eager module.
    test(eager, N).


test(Module, 1) ->
    Seq = [{atm,a}],
    Prgm = [],
    io:format("eval expression ~w~n   should result in {ok, a}~n", [Seq]),
    apply(Module, eval, [Seq, Prgm]);

test(Module, 2) ->
    Seq = [{match, {var, x}, {atm,a}},
	   {cons, {var, x}, {atm, b}}
	  ],
    Prgm = [],
    io:format("eval expression ~w~n   should result in {ok, [a|b]}~n", [Seq]),
    apply(Module, eval, [Seq, Prgm]);

test(Module, 3) ->
    Seq = [{match, {var, x}, {atm,a}},
	   {match, {var, y}, {cons, {var, x}, {atm, b}}},
	   {match, {cons, ignore, {var, z}}, {var, y}},
	   {var, z}
	  ],
    Prgm = [],
    io:format("eval expression ~w~n   should result in {ok, b}~n", [Seq]),
    apply(Module, eval, [Seq, Prgm]);

test(Module, 4) ->
    Seq = [{match, {var, x}, {atm, a}},
	   {switch, {var, x},
	    [{clause, {atm, b}, [{atm, ops}]},
	     {clause, {atm, a}, [{atm, yes}]}
	    ]}
	  ],
    Prgm = [],
    io:format("eval expression ~w~n   should result in {ok, yes}~n", [Seq]),
    apply(Module, eval, [Seq, Prgm]);

test(Module, 5) ->
    Seq = [{match, {var, x}, {cons, {atm, a}, {atm, []}}},
	   {switch, {var, x},
  	       [{clause, {atm, []}, [{atm, ops}]},
		{clause, {cons, {var, hd}, {var, tl}}, [{var, hd}]}
	    ]}
	  ],
    Prgm = [],
    io:format("testing switch expression, should result in {ok, a}~n", []),
    apply(Module, eval, [Seq, Prgm]);

test(Module, 6) ->
    Seq = [{call, append, [{atm, []}, {atm, []}]}
           ],
    Prgm = prg(),
    io:format("testing function application, should result in {ok, []}~n", []),
    apply(Module, eval, [Seq, Prgm]);

test(Module, 7) ->
    Seq = [{match, {var, x}, {cons, {atm, a}, {cons, {atm, b}, {atm, []}}}},
	   {match, {var, y}, {cons, {atm, c}, {cons, {atm, d}, {atm, []}}}},
	   {call, append, [{var, x}, {var, y}]}
	  ],
    Prgm = prg(),
    io:format("testing recursive function, should result in {ok, [a,b,c,d]}~n", []),
    apply(Module, eval, [Seq, Prgm]);

test(Module, 8) ->
    Seq = [{match, {var, x}, {cons, {atm, a}, {cons, {atm, b}, {atm, []}}}},
	   {call, nreverse, [{var, x}]}
	  ],
    Prgm =  prg(),
    io:format("nreverse of [a,b], should result in {ok, [b,a]}~n", []),
    apply(Module, eval, [Seq, Prgm]);

test(Module, 9) ->
    Seq = [{match, {var, x}, {cons, {atm, a}, {cons, {atm, b}, {atm, []}}}},
	   {match, {var, y}, {atm, []}},
	   {match, {var, f}, {lambda, [p], [y], [{cons, {var, p}, {var,y}}]}},
	   {call, map, [{var, f}, {var, x}]}
	  ],
    Prgm =  prg(),
    io:format("higher order,  Y = [], F = fun(P) -> [P|Y] end, map(F,[a,b]), should result in {ok, [[a] [b]]}~n", []),
    apply(Module, eval, [Seq, Prgm]).


prg() -> [
{nreverse, [x], 
    [{switch, {var, x}, 
           [{clause, {atm, []}, [{atm, []}]},

            {clause, {cons, {var, hd}, {var, tl}},
	     [{call, append, 
	       [{call, nreverse, [{var, tl}]}, 
		{cons, {var, hd}, {atm, []}}]}
	     ]}
           ]}]},

{append, [x, y],
    [{switch, {var, x}, 
           [{clause, {atm, []}, 
              [{var, y}]},

            {clause, {cons, {var, hd}, {var, tl}}, 
              [{cons, {var, hd}, {call, append, [{var, tl}, {var, y}]}}]}]
      }]},

{map, [f,x], 
    [{switch, {var, x}, 
           [{clause, {atm, []}, 
              [{atm, []}]},
            {clause, {cons, {var, hd}, {var, tl}}, 
              [{cons, {apply, {var, f}, [{var, hd}]}, {call, map, [{var, f}, {var, tl}]}}]}]
      }]}

].


bench() ->
    %% The defaultmodule is eager.
    %% The benchmark is executed 100 times.
    bench(eager, 100).


bench(Module, N) -> 
    Prgm = prg(), 

    List = {cons, {atm, 1}, 
	    {cons, {atm, 2}, 
	     {cons, {atm, 3}, 
	      {cons, {atm, 4}, 
	       {cons, {atm, 5}, 
		{cons, {atm, 6}, 
		 {cons, {atm, 7},
		  {cons, {atm, 8}, 
		   {cons, {atm, 9}, 
		    {cons, {atm, 10}, 
		     {cons, {atm, 11}, 
		      {cons, {atm, 12}, 
		       {cons, {atm, 13}, 
			{cons, {atm, 14}, 
			 {cons, {atm, 15}, 
			  {cons, {atm, 16}, 
			   {cons, {atm, 17}, 
			    {cons, {atm, 18},
			     {cons, {atm, 19}, 
			      {cons, {atm, 20}, 
			       {atm, []}}}}}}}}}}}}}}}}}}}}},

    Seq = [{match, {var, x}, 
                   List},
               {call, nreverse, [{var, x}]}
              ],

    L = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],

    K = lists:foldl(fun(X, A) -> {X, A} end, {}, L),

    T1 = time(N, fun() -> apply(Module, eval, [Seq, Prgm]) end),
    T2 = time(N, fun() -> nrevk(K) end),
    T3 = time(N, fun() -> nrevl(L) end),
    T4 = time(N, fun() -> nrevb(L) end),

    io:format("naive reverse of 20 elements~n~n   meta-interpreter ~w us~n~n   abstract machine ~w us~n~n   native lists  ~w us ~n~n   builtin append  ~w us ~n~n   ratio meta/abstr: ~w~n", [T1, T2, T3, T4, trunc(T1/T2)]).




time(N, F)->
    %% time in micro seconds
    {T, _} = timer:tc(fun() -> loop(N, F) end),
    T.

loop(N, Fun) ->
  if N == 0 -> ok; true -> Fun(), loop(N-1, Fun) end.


    

nrevl([]) ->
    [];
nrevl([H|T]) ->
    appl(nrevl(T), [H]).

appl([], Y) ->
    Y;
appl([H|T], Y) ->
    [H|appl(T,Y)].

nrevk({}) ->
    {};
nrevk({H, T}) ->
    appk(nrevk(T), {H, {}}).

appk({}, Y) ->
    Y;
appk({H, T}, Y) ->
    {H, appk(T,Y)}.



nrevb([]) ->
    [];
nrevb([H|T]) ->
    nrevb(T) ++ [H].
    
