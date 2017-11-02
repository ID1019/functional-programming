-module(lzw).

-compile(export_all).

-define(Alphabet, "abcdefghijklmnopqrstuvwxyz ").


test() ->
    test("this is a test to see if it works").

test(Text) ->
    io:format("~w~n", [Text]),
    Encoded = encode(Text),
    io:format("~p~n", [Encoded]),
    io:format("length of text: ~w~n", [length(Text)]),
    io:format("length of encoded: ~w~n", [length(Encoded)]),
    case decode(Encoded) of
	Text ->
	    true;
	_ ->
	    false
    end.
    

% Encode a text by gradually building the encodiong table. To start
% with, the table only contains the known alphabet. If a sequence of
% characters are not found in the table then it is added to the table
% given a new code.

encode([]) ->
    [];
encode([Char|Rest]) ->
    Table = table(),
    Word = [Char],
    {found, Code} = encode_word(Word, Table),
    encode(Rest, Word, Code, Table).

encode([], _Sofar, Code, _Table) ->
    [Code];
encode([Char|Rest], Sofar, Code, Table) ->

    %% This is the tricky part, we have read Sofar and now have Char as
    %% the next character. Notice that Sofar is coded in reverse order,
    %% this is ok - all strings in the table are in reverse order.

    Extended = [Char|Sofar],
    %% io:format("looking for ~s in ~p~n", [Extended, Table]),

    case encode_word(Extended, Table) of

	 {found, Ext} ->
	     %% If the extended word is found we continue to see if can
	     %% find an even longer string. We keep the code for the
	     %% extended word.

             %%io:format("found ~s~n", [Extended]),
	     encode(Rest, Extended, Ext, Table);

	 {notfound, Updated} ->
	    %% If the extended string is not found we output the code
	    %% for the word that we had read sofar.  We then lookup
	    %% the code for the charcter and continue with the rest of
	    %% the text.
	    
	    %% io:format("not found ~s~n", [Extended]),
	    %% io:format("output ~w~n", [Sofar]),
	    Word = [Char],
	    {found, Cd} = encode_word(Word, Table),
	    [Code | encode(Rest, Word, Cd, Updated)]
    end.

encode_word(Word, {N, Words}) ->
    %% This is where we do a lookup of the word and if not found adds
    %% it to the table. We increment N so that we know the code for
    %% the next sequence.

    %%io:format("looking for ~w in ~w~n", [Word, Words]),
    case lists:keysearch(Word, 1, Words) of
	{value, {Word, Code}} ->
	    {found, Code};
	false ->
	    %%io:format("adding ~w ~s~n", [N, lists:reverse(Word)]),
	    {notfound, {N+1, [{Word, N}|Words]}}
    end.


% Decoding is slightly trickier, it is almost straight forward but it
% has a catch. 

decode(Codes) ->
    Table = table(),
    decode(Codes, Table).

decode([], _) ->
    [];
decode([Code], {_, Words}) ->
    {value, {Word, Code}} = lists:keysearch(Code, 2, Words),    
    Word;
decode([Code | Codes], {N, Words}) ->
    %% If we have a code we will of course simply do a lookup using
    %% the table. We take for granted that the word si foudn in the 
    %% table,.

    %% io:format("decoding ~w~n", [Code]),
    {value, {Word, _}} = lists:keysearch(Code, 2, Words),

    %% We then also do a lookup for the next code and will almost
    %% certainly also find this in the table.
   
    [Next|_] = Codes,
    NextChar = case lists:keysearch(Next, 2, Words) of
		   {value, {[Char|_], _}} ->
		       %% If the next code is found, we know that the
		       %% encoder has read first Word and then the
		       %% first character of the next code. This would
		       %% then have been added to the table by the
		       %% encoder - we do the same.
                       Char;
		   false ->
		       %% This is the not so obvious part, if the next
		       %% code is not found - we know that the encoder
		       %% has read Word followed by the first
		       %% character in a sequence that is equal to
		       %% Word. So we return the first character in
		       %% Word.

		       io:format("could not find ~w, I bet it is ~w~n", [Next, N]),
		       
		       [Char|_] = Word,
		       Char
	       end,
    %% io:format("adding ~s as code ~w~n", [Word++[Char], N]),
    Word ++ decode(Codes, {N+1, [{Word++[NextChar], N}|Words]}).


    

table() ->   
    N = length(?Alphabet),
    Strings = lists:map(fun(Char) -> [Char] end, ?Alphabet),
    Map = lists:zip(Strings, lists:seq(1,N,1)),
    {N+1, Map}.

	    



