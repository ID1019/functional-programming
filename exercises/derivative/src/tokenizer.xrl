Definitions.

NUM        = [0-9]+
VAR        = [a-z]
EXP        = \^
WHITESPACE = [\s\t\n\r]

Rules.

{NUM}         : {token, {num, TokenLine, list_to_integer(TokenChars)}}.
{VAR}         : {token, {var, TokenLine, list_to_atom(TokenChars)}}.
{EXP}         : {token, {exp, TokenLine}}.
\(            : {token, {left, TokenLine}}.
\)            : {token, {right, TokenLine}}.
\+            : {token, {plus, TokenLine}}.
\*            : {token, {times, TokenLine}}.

{WHITESPACE}+ : skip_token.

Erlang code.

