-module(cell).

-export([start/1, read/1, write/2]).

start(V) ->
   %% things to do in mother process
   {cell, spawn_link(fun() -> init(V) end)}.

read({cell, C}) -> 
    C ! {read, self()},
    receive 
	{value, V} ->
	    V
    end.

write({cell, C}, W) -> 
    C ! {write, W, self()},
    receive 
	ok ->
	    ok
    end.

init(V) ->
   %% things to do in the child process
   cell(V).

cell(V) ->
  receive 
      {read, Pid} ->
	  Pid ! {value, V},
	  cell(V);
      {write, W, Pid} ->
	  Pid ! ok,
	  cell(W)
  end.
