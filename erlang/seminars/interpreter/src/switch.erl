-module(switch).


-compile(export_all).

test(X) ->
    Foo =  case X of
	      {f, B} -> 
		  B;
	      ok ->
		  bar
	   end,
    end.
    
	    
    
