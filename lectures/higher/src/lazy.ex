defmodule Lazy do

  

  def sum(r) do
    reduce(r, {:cont, 0}, fn(x,a) -> {:cont, x+a} end)
  end

  def take(r, n) do
    reduce(r, {:cont, {0,[]}},
      fn(x,{s,a}) ->
	if s == n do
	  {:halt, Enum.reverse(a)}
	else
	  {:cont, {s+1, [x|a]}}
	end
      end)
  end    


  def lazy(r,a) do
    reduce(r, {:cont, a},
      fn (x, a) ->
	{:suspend, x+a}
      end)
  end

  def reduce(_, {:halt, acc}, _fun) do
    {:halted, acc}
  end
  def reduce(range, {:suspend, acc}, fun) do
    {:suspended, acc, fn(cmd) -> reduce(range, cmd, fun) end}
  end
  def reduce({:range, from , to}, {:cont, acc}, fun) do
    if from == to do
      {:done, acc}
    else
      reduce({:range, from+1, to}, fun.(from, acc), fun)
    end
  end
      

end
