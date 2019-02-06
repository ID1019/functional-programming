-module(tmp).

-compile(export_all).

test(X) ->
    case X of
	hello -> 
	    Z = 2;
	goodbye ->
	    Z = 3
    end,
    Z.



	

