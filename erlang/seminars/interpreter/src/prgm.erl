-module(prgm).

-compile(export_all).


lookup(Id, Prg) ->
    lists:keyfind(Id, 1, Prg).


		       
