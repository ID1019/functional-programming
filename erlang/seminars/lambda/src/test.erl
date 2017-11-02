-module(test).

-compile(export_all).


append(A, B) ->
   Append = fun(X, Y, F) -> 
             case X of 
               [] -> Y;
               [H|T] -> [H | F(T,Y,F) ]
             end
           end,
   Append(A, B, Append).
