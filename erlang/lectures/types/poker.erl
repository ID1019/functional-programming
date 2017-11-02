-module(poker).

-compile(export_all).

-type hand() :: hand(cards:card()).

-type hand(C) :: {hand, C, C, C, C, C}.


-spec flush() -> hand().

flush() ->
    C1 = cards:heart(1),
    C2 = cards:heart(2),
    C3 = cards:heart(3),
    C4 = cards:heart(4),
    C5 = cards:heart(5),
    {hand, C1, C2, C3, C4, C5}.
    


