defmodule Env do

  def new() do
    []
  end

  def add(id, str, env) do
    [{id, str}|env]
  end

  def remove(ids, env) do
    List.foldr(ids, env,
      fn (id, env) ->
	List.keydelete(env, id, 0)
      end)
  end

  def args(prm, args) do
    List.zip([prm, args])
  end

  def args(prm, args, env) do
    List.zip([prm, args]) ++ env
  end

  def lookup(id, env) do
    List.keyfind(env, id, 0)
  end
	
  def closure(ids, env) do
    List.foldr(ids, [],
      fn (id, acc) -> 
	case acc do
	  :error ->
	    :error
	  cls -> case lookup(id, env) do
		   {id, value} ->
		     [{id, value}|cls]
		   nil ->
		     :error
		 end
	end
      end)
  end

end

