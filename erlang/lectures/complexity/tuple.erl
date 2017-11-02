-module(tuple).

-export([emplty/0, lookup/2, modify/3, delete/2, insert/3]).

emply() ->
    {}.

lookup(K, Tuple) -> 
    element(K, Tuple).

modify(K, V, Tuple) -> 
    setelement(K, Tuple, {value, V}).

delete(K, Tuple) -> 
    setelement(K, Tuple, false).

insert(K, V, Tuple) -> 
    erlang:insert_element(K, Tuple, {value, V}).



