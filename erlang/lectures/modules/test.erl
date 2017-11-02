-module(test).

-compile(export_all).


-foo("this is it").

-bar([gurka, 42]).

-define(pi, 3.14).


%-define(TRACE(Msg), io:format("trace: ~s~n", [Msg])).
-define(TRACE(Msg), ok).


-ifdef(debug).
-define(DEBUG(Msg), io:format("debug: ~s~n", [Msg])).
-else.
-define(DEBUG(Msg), true).
-endif.



area(R) -> 
    ?TRACE("area called"),
    ?DEBUG("even more information"),
    R*math:pow(?pi,2).






     
