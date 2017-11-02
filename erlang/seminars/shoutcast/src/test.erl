

-module(test).

-export([app/2]).


app([], Y) ->
    Y;
app([H|T], Y) ->
    app(T, [H|Y]).

