-module(test).

-compile(export_all).



foo(X) ->
  try length(X) of
     0 -> empty;
     _ -> something
  catch 
      error:Error -> {arghh, Error}
  end.


sum([]) -> 0;
sum([H|T]) -> H + sum(T);
sum(What) -> error({argh, "What is this", What}).
