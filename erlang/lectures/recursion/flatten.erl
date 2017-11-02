-module(flatten).

-compile(export_all).


flatten([]) -> [];
flatten([[]|T])-> flatten(T);
flatten([[H|L]|T]) -> [H | flatten([L|T])};
flatten([H|T]) -> [H|flatten(T)].

