-module(env).

-compile(export_all).

new() ->
    [].

add(Id, Str, Env) ->
    [{Id, Str}|Env].

args(Prm, Args) ->
    lists:zip(Prm, Args).

args(Prm, Args, Env) ->
    lists:append(lists:zip(Prm, Args), Env).

lookup(Id, Env) ->
    lists:keyfind(Id, 1, Env).
	
closure(Ids, Env) ->    
    lists:foldr(fun(Id, Acc) -> 
			case Acc of 
			    error ->
				error;
			    Cls -> case env:lookup(Id, Env) of
				       {Id, Value} ->
					   [{Id, Value}|Cls];
				       false ->
					   error
				   end
			end
		end,
		[],
		Ids).


