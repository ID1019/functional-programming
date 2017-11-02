-module(test).

-compile(export_all).

test() ->
    fib(a).

%-spec fib(integer()) -> integer().

fib(0) -> 0;
fib(1) -> 1;
fib(N)  -> 
    fib(N-1) + fib(N-2).

		     


	  



     


    
