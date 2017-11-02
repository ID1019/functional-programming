-module(higher).

-compile(export_all).

%%  Terms and patterns:
%%
%%     {atm, <id>}
%%     {var, <id>}
%%     ignore   (only for patterns)
%%     {cons, <term>, <term>}
%%

%%  Expressions:
%%     
%%     <term>
%%     {call, <id>, [<expr>|...]}
%%     {apply, <expr>, [<expr>|...]}
%%     {switch, <expr>, <clauses>}
%%     {fun, [<free var>|...], [<param var>| ...], <sequence>}

%%  Clauses
%%
%%     [] 
%%     [{clause, <pattern>, <sequence>}| <clauses>] 

%%  Sequence:
%% 
%%     [<expression>]
%%     [{match, <pattern>, <expression>} | <sequence> ]



%% eval(Sequence) -> Term | fail

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
    end;

eval_expr({apply, Expr, Exprs}, Env, Prg)  ->
    case eval_expr(Expr, Env, Prg) of
	error ->
	    error;
	{ok, {abs, Par, Seq, Closure}} ->
	    case eval_args(Exprs, Env, Prg) of 
		error ->
		    error;
		Args  ->
		    New = env:args(Par, Args, Closure),
		    eval_seq(Seq, New, Prg)
	    end
    end;


eval_expr({lambda, Par, Free, Seq}, Env, _Prg) ->
    case closure(Free, Env) of
	error ->
	    error;
	{ok, Closure} ->
	    {ok, {abs, Par, Seq, Closure}}
    end.

closure(Free, Env) ->
    New = env:new(),
    closure(Free, Env, New).

closure([], _, New) ->
    {ok, New};
closure([F|Free], Env, New) ->
    case env:lookup(F, Env) of
	false ->
	    error;
	{F, Str} ->
	    closure(Free, Env, env:add(F, Str, New))
    end.
	    


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

     


