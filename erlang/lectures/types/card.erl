-module(card).

-compile(export_all).

-type suit() :: spade | heart | diamond | clubs.
-type value() :: 1..13.
-type card() :: {card, suit(), value()}.

-spec suit(card()) -> suit().

suit({card, Suit, _}) -> Suit.

value({card, _, Value}) -> Value.
     
