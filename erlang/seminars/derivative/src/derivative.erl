-module(derivative).

-compile(export_all).

-type constant() :: {const, number()} | {const, atom()} .

-type literal() ::  constant() | {var, atom()}.

-type expr() :: {exp, constant(), literal(), integer()} | {mul, constant(), literal()} | literal().

-spec deriv(expr(), atom()) -> expr().

test() ->
    Test = {add, {mul, {const, 4}, {exp, {var, x}, {const, 2}}},
	         {add, {mul, {const, 3}, {var, x}}, 
	               {const, 42}
                 }
           },
    Der = deriv(Test, x),
    Simpl = simplify(Der),
    printp(Test), io:format("~n",[]),
    printp(Der), io:format("~n",[]),
    printp(Simpl), io:format("~n",[]),
    {Der, Simpl}.



deriv({const, _}, _) ->
    {const, 0};
deriv({var, V}, V) ->
    {const, 1};
deriv({var, Y}, _) ->
    {var, Y};
deriv({mul, E1, E2}, V) ->
    {add, {mul, deriv(E1, V), E2}, {mul, E1, deriv(E2, V)}};
deriv({exp, {var, V}, {const, C}}, V) ->
    {mul, {const, C}, {exp, {var, V}, {const, C-1}}};
deriv({add, E1, E2}, V) ->
    {add, deriv(E1, V), deriv(E2, V)}.


simplify({const, C}) ->
    {const, C};
simplify({var, C}) ->
    {var, C};
simplify({exp, E1, E2}) ->
    case simplify(E2) of
	{const, 0} ->
	    {const, 1};
	{const, 1} ->
	    simplify(E1);
	S2 -> 
	    case simplify(E1) of
		{const, 0} ->
		    {const, 0};
		{const, 1} ->
		    {const, 1};
		S1 ->
		    {exp, S1, S2}
	    end
    end;

simplify({mul, E1, E2}) ->
    case simplify(E1) of
	{const, 0} ->
	    {const, 0};
	{const, 1} ->
	    simplify(E2);
	S1 ->
	    case simplify(E2) of
		{const, 0} ->
		    {const, 0};
		{const, 1} ->
		    S1;
		S2 ->
		    {mul, S1, S2}
	    end
    end;
simplify({add, E1, E2}) -> 
    case simplify(E1) of
	{const, 0} ->
	    simplify(E2);
	S1 ->
	    case simplify(E2) of
		{const, 0} ->
		    S1;
		S2 ->
		    {add, S1, S2}
	    end
    end.




printp({const, C}) ->
    io:format("~w",[C]);
printp({var, V}) ->
    io:format("~w",[V]);
printp({exp, E1, E2}) ->
    printpp(E1),
    io:format("^",[]),
    printpp(E2);
printp({mul, E1, E2}) ->
    printpp(E1),
    io:format(" * ",[]),
    printpp(E2);
printp({add, E1, E2}) ->
    printp(E1),
    io:format(" + ",[]),
    printp(E2).


printpp({const, C}) ->
    io:format("~w",[C]);
printpp({var, V}) ->
    io:format("~w",[V]);
printpp({exp, E1, E2}) ->
    printpp(E1),
    io:format("^",[]),
    printpp(E2);
printpp({mul, E1, E2}) ->
    printpp(E1),
    io:format(" * ",[]),
    printpp(E2);
printpp({add, E1, E2}) ->
    io:format("(",[]),
    printp(E1),
    io:format(" + ",[]),
    printp(E2),
    io:format(")",[]).






