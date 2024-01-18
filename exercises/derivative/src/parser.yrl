Nonterminals expr.
Terminals num var left right plus times exp.
Rootsymbol expr.


Left 300 plus.
Left 400 times.
Left 600 exp.

expr -> num : value('$1').
expr -> var : name('$1').
expr -> expr plus expr : {add, '$1', '$3'}.
expr -> expr times expr : {mul, '$1', '$3'}.
expr -> expr exp expr : {exp, '$1', '$3'}.
expr -> left expr right : '$2'.

Erlang code.

value({num, _, Value}) -> Value.
name({var, _, Name}) -> Name.




