-module(recursion).

-compile(export_all).



test(A, B) ->
    Append = fun(App, X, Y) ->
		     case X of
			 [] ->
			     Y;
			 [H|T] -> [H|App(App, T, Y)]
		     end
	     end,
    Append(Append, A, B).
		     
			     

