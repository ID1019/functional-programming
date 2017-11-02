-module(church).

-compile(export_all).

%% A Church number N is a function of two arguments that applies the
%% first argument N tims to its second argument.

church(0) -> fun(_, Y) -> Y end;
church(N) -> fun(F, X) -> F( (church(N-1))(F, X) ) end.

%% We can convert a Church number to an integer using the function
%% fun(X) -> X + 1 end and let the Church number apply this to zero. 

integer(Church) -> Church(fun(X) -> 1 + X end , 0).


%% The successor of N is a functon that takes two arguments and
%% applies the first argument to the second N times usng the Church
%% number and the adds another application.

succ(N) -> fun(F, X) -> F(N(F, X)) end.


%% Add A and B

%% Adding N and M is acheived by returning a function that takes two
%% argumnets and then using N and M to apply the first argumnet to the
%% second N plus M times.

add(N, M) ->  fun(F, X) -> N(F, M(F,X)) end.


mul(N, M) ->  fun(F, X) -> N(fun(Y) -> M(F, Y) end, X) end.
		       

%% The predecessor of N is tricky but you have to know that in Church numbers
%% the predecessor of zero is zero i.e. there are no negative numbers.

							      
pred(N) ->  fun(F, X) -> 
               ( N(                                        % N is a Church numeral
                    fun(G) -> fun(H) -> H(G(F)) end end,   %  apply this function N times 
                    fun(_) -> X end)                       %  to this  function 
               )                                           % the reusuting function 
               (fun(U) -> U end)                           % is applied to the identity function
            end.


%%% How does this work? Assume we call pred(Four) wher Four is the
%%% Church numeral for 4. Then we will return a function, call it
%%% Three, that as expected takes two argumnets, a function F and and
%%% argumnet X. What happens if we now use this function and call
%%% Three(f, 0)? 
%%%
%%% First we will call Four, with a strange looking function and a
%%% function that ignores its argumnet. Let's apply this functions
%%% ones, what do we get?
%%%
%%%    fun(H) -> H( fun(_) -> 0 end (f)) end 
%%%
%%% or
%%%
%%%    fun(H) -> H(0) end
%%% 
%%% Ok, so now we three more goes to apply the strange function. 
%%%
%%%    fun(H) -> H( fun(H') -> H'(0) end (f)) end 
%%%
%%% or
%%%  
%%%   fun(H) -> H( f(0) ) end     
%%%
%%% Hmmm, let's give it another shot:
%%%
%%%   fun(H) -> H( fun(H') -> H'( f(0) ) end end (f)) end 
%%%
%%% or
%%%  
%%%   fun(H) -> H( f(f(0)) ) end 
%%%
%%% Ok, a last time:
%%%
%%%   fun(H) -> H( fun(H') -> H'( f(f(0)) ) end end (f) end 
%%%
%%% or
%%%  
%%%   fun(H) -> H(f(f(f(0))) ) end 
%%% 
%%% And now we take this function and apply it to the identity
%%% function fun(U) -> U end. The result is:
%%%
%%%    f(f(f(0)))
%%%
%%% and this is of course execty what we would like to see from
%%% (pred(Four))(f, 0). 
%%%
%%% I you think this was complicated you're right!

minus(M, N) -> N(fun(X) -> pred(X) end, M).

%%% this will not work since if_then_else will evaluate all arguments
manus(M, N) -> 
  if_then_else(zero(N), M, manus(M, pred(N))).

%%% this works but now we use the Erlang case statement and thereby
%%% avoid the eager evaluation

menos(M, N) ->
    case check(zero(N)) of
	true ->
	    M;
	false ->
	    menos(pred(M), pred(N))
    end.



%%% Boolean values are encoded as follows.

true() -> fun(X,_) -> X end.

false() -> fun(_,Y) -> Y end.

%%% and can be check by:

check(Bool) -> Bool(true, false).
    
%%% If then else ... but it will evaluate all arguments!

if_then_else(If,Then,Else) -> If(Then, Else).

	    

bool_and(A,B) -> A(B,A). 
bool_or(A,B) -> A(A,B).     
bool_not(A) -> fun(X,Y) -> A(Y, X) end.
			    
		  
%%% Predicates

zero(N) -> N(fun(_) -> false() end, true()).
		      
leq(M,N) ->  zero(minus(M, N)).



fib(N) ->
   Fib = fun(X,F) -> 
		 case X of
		     0 -> 0;
		     1 -> 1;
		     D -> F(D-1,F) + F(D-2,F)
		 end
	 end,
    Fib(N, Fib).

			      
			     
		  
    
     
