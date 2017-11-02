-module(eager).

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
%%   {lambda, <param>, <free>, <sequence>} : {closure, <param>, <sequence>, <environment>}
    


%% eval(Sequence, Env) -> {ok, Str} | fail

eval(Seq, Prg) ->
    eval_seq(Seq, env:new(), Prg).

eval_seq([Exp], Env, Prg) ->
    eval_expr(Exp, Env, Prg);

eval_seq([{match, Ptr, Exp}|Seq], Env, Prg) ->
    case eval_expr(Exp, Env, Prg) of
	error -> 
	    error;
	{ok, Str}  ->
	    case eval_match(Ptr, Str, Env) of
		fail ->
		    error;
		{ok, Env2} ->
		    eval_seq(Seq, Env2, Prg)
	    end
    end.


eval_expr({atm, Id}, _, _) ->
    {ok, Id};

eval_expr({var, Id}, Env, _) ->
    case env:lookup(Id, Env) of
	false ->
	    error;
	{Id, Str} ->
	    {ok, Str}
    end;

eval_expr({cons, He, Te}, Env, Prg) ->
    case eval_expr(He, Env, Prg) of
	error ->
	    error;
	{ok, StrH} ->
	    case eval_expr(Te, Env, Prg) of
		error ->
		    error;
		{ok, StrT} -> 
		    {ok, [StrH|StrT]}
	    end
    end;

eval_expr({switch, Expr, Cls}, Env, Prg) ->
    case eval_expr(Expr, Env, Prg) of 
	error ->
	    error;
	{ok, Str} ->
	    eval_cls(Str, Cls, Env, Prg)
    end;

eval_expr({lambda, Par, Free, Seq}, Env, _Prg) ->
    %% Assume we represent a fun-expression as a list of parameter
    %% identifiers, a list of free identifiers and a sequence.
    case env:closure(Free, Env) of
	error ->
	    error;
	Closure ->
	    {ok, {closure, Par, Seq, Closure}}
    end;

eval_expr({apply, Expr, Args}, Env, Prg) ->
    case eval_expr(Expr, Env, Prg) of 
	{ok, {closure, Par, Seq, Closure}} ->
	    case eval_args(Args, Env, Prg) of
		error ->
		    error;
		EvaluatedArgs ->
		    New = env:args(Par, EvaluatedArgs, Closure),
		    eval_seq(Seq, New, Prg)
	    end;
	error ->
	    error
    end;
  
eval_expr({call, Id, Exprs}, Env, Prg) when is_atom(Id) ->
    case eval_args(Exprs, Env, Prg) of 
	error ->
	    error;
	Args  ->
	    case prgm:lookup(Id, Prg) of
		false->
		    error;
		{Id, Prm, Seq} ->
		    New = env:args(Prm, Args),
		    eval_seq(Seq, New, Prg)
	    end
    end.


%%% Evaluate a match expression, return fail or {ok, Env} where Env is
%%% a possibly updated environment.

eval_match({atm, Id}, Id, Env) ->    
    {ok, Env};
eval_match({var, Id}, Str, Env) -> 
    case env:lookup(Id, Env) of
	false ->
	    {ok, env:add(Id, Str, Env)};
	{Id, Str} ->
	    {ok, Env};
	{Id, _} ->
	    fail
    end;
eval_match(ignore, _, Env) ->
    {ok, Env};
eval_match({cons, Hp, Tp}, [Ht|Tt], Env) -> 
    case eval_match(Hp, Ht, Env) of
	fail ->
	    fail;
	{ok, Env2} ->
	    eval_match(Tp, Tt, Env2)
    end;
eval_match(_, _, _) -> 
    fail.

eval_cls(_, [], _, _) ->
    error;
eval_cls(Trm, [{clause, Ptr, Seq} | Cls], Env, Prg) ->
    case eval_match(Ptr, Trm, Env) of
	fail ->
	    eval_cls(Trm, Cls, Env, Prg);
	{ok, Env2} ->
	    eval_seq(Seq, Env2, Prg)
    end.

%%% Evaluate a list of expressions, if any expression evaluates to an
%%% error then return error, otherwise return a list of the evaluated
%%% expressions.

eval_args([], _, _) ->
    [];
eval_args([Expr|Exprs], Env, Prg) ->
    case eval_expr(Expr, Env, Prg) of
	error ->
	    error;
        {ok, Str} -> 
	    case eval_args(Exprs, Env, Prg) of
		error ->
		    error;
		Strs -> 
		   [Str | Strs]
	    end
    end.

     


