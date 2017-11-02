SOURCE:

    X = 3, Y = {a, f}, {X, {Y, c}}

     X = 3, Y = [a|f], [X|[Y|c]]

Represented as:
 
 [{match, {var, x}, {atm, 3}}, 
  {match, {var, y}, {cons, {atm, a}, {atm, f}}},
  {cons, {var, x}, {cons, {var, y}, {atm, c}}}]

Evaluates to:

  [3 | [[a|f] | c]]

  {a, {b, {c, []}}}  ----  [a | [b | [c | []]]] ==== [a,b,c]


SOURCE:

X = 45, F = fun(Y) -> {Y, X} end,   F(45)


foo(X) 

foobar(Z) -> X = 34, Z(13).

{match, {var, x}, {atm, 45}},
{match, {var, f}, {lambda, [y] , [{cons, {var, y}, {atm, 42}}]}},
{apply, {var, f}, [{atm,42}]}


 
	     

 

		    


	 
        
			   
			       
 


