-module(lazy).

-compile(export_all).

%%  Terms and patterns:
%%
%%     {atm, <id>}
%%     {var, <id>}
%%     ignore   (only for patterns)
%%     {cons, <term>, <term>}
%%     {lambda, <param>, <free>, <sequence>}
%%

%%  Expressions:
%%     
%%     <term>
%%     {call, <id>, [<expr>|...]}
%%     {switch, <expr>, <clauses>}

%%  Clauses
%%
%%     [] 
%%     [{clause, <pattern>, <sequence>}| <clauses>] 

%%  Sequence:
%% 
%%     [<expression>]
%%     [{match, <pattern>, <expression>} | <sequence> ]

%%  Expressions are evaluated to data structures 
%% 
%%   {atm, <id>} : as the Erlang atom <id>
%%   {cons, <head>, <tail>} : as [ <head> | <tail> ]
%%   {lambda, <param>, <free>, <sequence>} : {closure, <param> <sequence>, <environment>}



%% eval(Sequence, Prgm) -> {ok, Str} | fail

eval(Seq, Prgm) ->
   case eval_seq(Seq, env:new(), Prgm) of
       {ok, Lazy} ->
	   force(Lazy, Prgm);
       error ->
	   error
   end.

%%% In order to get any sensible output we need to force the
%%% evaluation to proceed. 

force({lazy, Lazy, Closure}, Prgm) ->
    case eval_expr(Lazy, Closure, Prgm) of
	{ok, Evaluated} ->
	    force(Evaluated, Prgm);
	error ->
	    error
    end;

force({closure, Par, Seq, Closure}, _Prgm) ->
    {ok, {closure, Par, Seq, Closure}};

force([H|T], Prgm) ->
    case force(H, Prgm) of
	{ok, EH} ->
	    case force(T, Prgm) of
		{ok, ET} ->
		    {ok, [EH|ET]};
		error ->
		    error
	    end;
	error ->
	    error
    end;

force(Atom, _) ->
    %% It should the be an atom.
    {ok, Atom}.



eval_seq([Exp], Env, Prgm) ->
    eval_expr(Exp, Env, Prgm);

eval_seq([{match, Ptr, Exp}|Seq], Env, Prgm) ->

    %% The difference between eager and lazy is that we here delay the
    %% evaluation of the expression. We do the pattern matching with the 
    %% pattern and the term expression.

    case eval_match(Ptr, {lazy, Exp, Env}, Env, Prgm) of 
	fail -> 
	    error; 
	error ->
	    %% matching can now cause an error
	    error;
	{ok, Env2} ->
	    eval_seq(Seq, Env2, Prgm) 
    end.


eval_expr({atm, Id}, _, _) ->
    {ok, Id};

eval_expr({var, Id}, Env, _) ->
    case env:lookup(Id, Env) of
	false ->
	    fail;
	{Id, Str} ->
	    {ok, Str}
    end;

eval_expr({cons, He, Te}, Env, _) ->
    %% Only do as little as possible.
    {ok, [{lazy, He, Env} | {lazy, Te, Env}]};

eval_expr({switch, Expr, Cls}, Env, Prgm) ->
    %% Delay the evaluation of the expressions.
    eval_cls({lazy, Expr, Env}, Cls, Env, Prgm);

eval_expr({lambda, Par, Free, Seq}, Env, _Prg) ->
    case env:closure(Free, Env) of
	error ->
	    io:format("closure error ~w ~w~n", [Free, Env]),	    
	    error;
	Closure ->
	    {ok, {closure, Par, Seq, Closure}}
    end;

eval_expr({apply, Expr, Args}, Env, Prg) ->
    case eval_expr(Expr, Env, Prg) of
	{ok, Evaluated} ->
	    case force(Evaluated, Prg) of 
		{ok, {closure, Par, Seq, Closure}} ->
		    Closures  = lists:map(fun(Arg) -> {lazy, Arg, Env} end, Args),
		    New = env:args(Par, Closures, Closure),
		    eval_seq(Seq, New, Prg);
		error ->
		    error
	    end;
	error ->
	    error
    end;

eval_expr({call, Id, Exprs}, Env, Prgm) when is_atom(Id) ->
    case prgm:lookup(Id, Prgm) of
	false->
	    fail;
	{Id, Prm, Seq} ->
	    %% delay the evaluation of expressions
	    Delay = lists:map(fun(E) -> {lazy, E, Env} end, Exprs),
	    New = env:args(Prm, Delay),
	    eval_seq(Seq, New, Prgm)
    end.


%%% Evaluate a match expression, return fail or {ok, Env} where Env is
%%% a possibly updated environment.

eval_match({atm, Id}, Id, Env, _) ->    
    {ok, Env};

eval_match({var, Id}, Str, Env, Prgm) -> 
    case env:lookup(Id, Env) of
	false ->
	    {ok, env:add(Id, Str, Env)};
	{Id, Str} ->
	    {ok, Env};
	{Id, {lazy, Lazy, Closure}} ->
	    %% This is new, the vaiable coudl be bound 
	    case eval_expr(Lazy, Closure, Prgm) of
		{ok, Evaluated} ->
		    %% This is where we would like to remember the
		    %% evaluated result. If we look-up the variable
		    %% again, we will have to evaluate it ones more.
		    case eval_equal(Evaluated, Str, Env, Prgm) of
			true ->
			    {ok, Env};
			false ->
			    fail
		    end;
		error ->
		    error
	    end;
	{Id, _} ->
	    fail
    end;

eval_match({cons, Hp, Tp}, [Ht|Tt], Env, Prgm) -> 
    case eval_match(Hp, Ht, Env, Prgm) of
	fail ->
	    fail;
	error ->
	    %% matching can now cause an error
	    error;
	{ok, Env2} ->
	    eval_match(Tp, Tt, Env2, Prgm)
    end;

eval_match(ignore, _, Env, _) ->
    {ok, Env};

eval_match(Pat, {lazy, Lazy, Closure}, Env, Prgm) ->
    %% The data structure could be a lazy expression.
    case eval_expr(Lazy, Closure, Prgm) of
	error ->
	    %% This is a new situation, pattern-matching can cause an error,
	    %% any function that calls eval_match must now also handle
	    %% this situation.
	    error;
	{ok, Str} ->
	    eval_match(Pat, Str, Env, Prgm)
    end;
	    
eval_match(_, _, _, _) -> 
    fail.

%%% This is new, we need to check if two data structures are equal. We
%%% need to do this ourselves since the data structures could contain
%%% lazy expressions. 

eval_equal(Str, Str, _Env, _Prgm) ->
    %% This is the easy case, the two structures are equal.
    true;
eval_equal([H1|T1], [H2|T2], Env, Prgm) ->
    %% Also straight forward, if both data structures are cons cells
    %% then we continue. 
    case eval_equal(H1, H2, Env, Prgm) of
	true ->
	    case eval_equal(T1, T2, Env, Prgm) of	    
		ok ->
		    ok;
		false ->
		    false
	    end;
	false ->
	    false
    end;
eval_equal(Str, {lazy, Lazy, Closure}, Env, Prgm) ->
    %% Second struture is a lazy expression, evaluate and proceed. 
    case eval_expr(Lazy, Closure, Prgm) of
	{ok, Evaluated} ->
	    eval_equal(Str, Evaluated, Env, Prgm);
	error ->
	    %% The equality test could also cause an error!
	    error
    end;
eval_equal({lazy, Lazy, Closure}, Str, Env, Prgm) ->
    %% First struture is a lazy expression, same as above.
    case eval_expr(Lazy, Closure, Prgm) of
	{ok, Evaluated} ->
	    eval_equal(Str, Evaluated, Env, Prgm);
	error ->
	    error
    end;
eval_equal(_, _, _, _) ->
    false.


%%% Selecting the right clause in a sequence of clauses.

eval_cls(_, [], _, _) ->
    error;

eval_cls(Trm, [{clause, Ptr, Seq} | Cls], Env, Prgm) ->
    case eval_match(Ptr, Trm, Env, Prgm) of
	fail ->
	    eval_cls(Trm, Cls, Env, Prgm);
	error ->
	    %% matching can now cause an error
	    error;
	{ok, Env2} ->
	    eval_seq(Seq, Env2, Prgm)
    end.


eval_args([], _, _) ->
    [];

eval_args([Expr|Exprs], Env, Prgm) ->
    case eval_expr(Expr, Env, Prgm) of
	fail ->
	    fail;
        {ok, Str} -> 
	    case eval_args(Exprs, Env, Prgm) of
		fail ->
		    fail;
		Strs -> 
		    [Str | Strs]
	    end
    end.

     


