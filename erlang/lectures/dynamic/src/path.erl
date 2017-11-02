%% shortest path

-module(path).

-compile(export_all).

graph() ->  [{a, b, 4}, 
	     {a, c, 2},
	     {b, c, 1},
	     {b, d, 2},
	     {b, e, 4},
	     {c, f, 1},
	     {c, h, 5},
	     {d, g, 2},
	     {e, f, 2},
	     {f, h, 1},
	     {g, h, 2}].


%% directed and no cycles : DAG

shortest(From, From, _) ->
    {0, []};
shortest(From, To, Graph) ->
    Next = next(From, Graph),
    Distances = distances(Next, To,Graph),
    select(Distances).


next(From, Graph) ->
    lists:foldl(fun({F,T,D}, N) -> 
			if F == From  -> [{T,D}|N];
			   true -> N
			end
		end,
		[],Graph).

distances(Next, To, Graph) ->
    lists:map(fun({T,D}) ->  
		      case shortest(T, To, Graph) of
			  {inf, na} -> 
                              {inf, na};
			{N, Path} ->
			      {D+N, [T|Path]}
		      end
		end, Next).    

select(Distances) ->
    lists:foldl(fun(S, A) ->
			{D,_}= S,
			{Ad,_}=A,
			if 
			    D < Ad ->
				S;
			    true ->
				A
			end
		end,
		{inf, na},
		Distances).
    

%% DAG with memorization

dynamic(From, To, Graph) ->
    Mem = new(),
    shortest(From, To, Graph, Mem).

check(From, To, Graph, Mem) ->
    case lookup(From, Mem) of
	{From, Solution} ->
	    {Solution, Mem};
	false ->
	    shortest(From, To, Graph, Mem)
    end.

shortest(From, From, _, Mem) ->
    {{0, []}, Mem};
shortest(From, To, Graph, Mem1) ->
    Next = next(From, Graph),
    {Distances, Mem2} = distances(Next, To,Graph, Mem1),
    Shortest =  select(Distances),
    Mem3 = store(From, Shortest, Mem2),
    {Shortest, Mem3}.

distances(Next, To, Graph, Mem) ->
    lists:foldl(fun({T,D}, {Dis,M}) ->	
			{{N, Path}, Upd} = check(T, To, Graph, M),
			{[{D+N, [T|Path]}|Dis], Upd}
		end, 
		{[],Mem},
		Next).    

new() ->
    gb_trees:empty().
    
store(K,V, Mem) ->
    gb_trees:insert(K, V, Mem).
    
lookup(K, Mem) ->
    case gb_trees:lookup(K, Mem) of none ->  false; {value, Value} ->  {K, Value}  end.



%% if there are (or could be) cycles, we could run into a loop 


			  

		       
    


