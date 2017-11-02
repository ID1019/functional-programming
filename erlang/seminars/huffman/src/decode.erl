
decode([], _Table) ->
     [];
decode(Seq, Table) ->
     {Char, Rest} = decode_char(Seq, 1, Table),
     [Char | decode(Rest, Table)].

decode_char(Seq, N, Table) ->
     {Code, Rest} = lists:split(N, Seq),
     case lists:keysearch(Code, 2, Table) of
       {value, {Char, Code}} ->
             {Char, Rest};
       false ->
             decode_char(Seq, N+1, Table)
      end.
