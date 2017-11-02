-module(cards).

-export_type([card/0, deck/0, suit/0, value/0]).


-compile(export_all).

-opaque suit() :: spade | heart | diamond | clubs.
-opaque value() :: 1..13.
-opaque card() :: {card, suit(), value()}.

-opaque deck() :: list(card()).

%-spec suit(cards:card()) -> cards:suit().

suit({card, Suit, _}) -> Suit.

%-spec value(cards:card()) -> cards:value().

value({card, _, Value}) -> Value.

     
%-spec heart(42..44) -> gurka.

heart(N) when (N >= 1) and (N =< 13) ->  {card, heart, N}.

