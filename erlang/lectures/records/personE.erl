-module(personE).

-compile(export_all).


new(Name, Age) ->
    {person,Name,Age,make_ref()}.

name({person,_,_,_} = P) ->
    case P of
        {person,rec0,_,_} ->
            rec0;
        _ ->
            error({badrecord,person})
    end.

age({person,_,Age,_}) ->
    Age.



