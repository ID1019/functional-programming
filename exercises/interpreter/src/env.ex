defmodule Env do

  def new() do
    []
  end

  def add(id, str, env) do
    [{id, str}|env]
  end

  def args(prm, args) do
    List.zip([prm, args])

  end

  def args(prm, args, Env) do
    List.zip(prm, args) ++ Env
  end

  def lookup(id, env) do
    List.keyfind(env, id, 0)
  end
	
  def closure(ids, env) do
    List.foldr(fn (id, acc) -> 
      case acc do
	:error ->
	  :error
	cls -> case lookup(id, env) do
		 {id, Value} ->
		   [{id, Value}|cls]
		 false ->
		   :error
	       end
      end
    end,
      [],
      ids)
  end

end

