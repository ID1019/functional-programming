-module(test).

-compile(export_all).

-record(person, {name, age}).


test() ->
    P = #person{name="Sune", age=28},
    foo(P, #person.name).


foo(P, Field) ->
    element(Field, P). 
