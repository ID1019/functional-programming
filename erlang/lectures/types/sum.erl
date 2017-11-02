-module(sum).

-compile(export_all).

test() ->
   L = [1,2,3,4],
   sum(L).

-spec sum(list()) -> integer().

sum([]) -> 0;
sum([H|T]) -> H + sum(T).


% -spec bar(integer()) -> [atom()].

bar(1) -> [a,b];
bar(2) -> [d,e].
     

-type foo() :: gurka | tomat.

-spec fruit(foo()) -> boolean().

fruit(gurka) -> false;
fruit(tomat) ->  true.
     

