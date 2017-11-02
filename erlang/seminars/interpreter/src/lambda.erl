eval_expr({lambda, Param, Seq}, _Env, _Prg) ->
    io:format("lambda evaluated~n",[]),    
    {ok, {funl, Param, Seq}};

eval_expr({apply, Expr, Exprs}, Env, Prg) ->
    case eval_expr(Expr, Env, Prg)  of
	{ok, {funl, Prm, Seq}} ->
	    case eval_args(Exprs, Env, Prg) of
		error -> 
		    io:format("no, arguments not evalueted~n",[]),
		    error;
		Args -> 
		    io:format("yes, arguments evalueted~n",[]),
		    Updated = env:args(Prm, Args, Env),
		    eval_seq(Seq, Updated, Prg)
	    end;
	_ ->
	    io:format("no, lambda not evalueted~n",[]),
	    error
    end;
