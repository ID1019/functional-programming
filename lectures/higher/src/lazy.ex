defmodule Lazy do

  
  def sum(r) do
    reduce(r, {:cont, 0}, fn(x,a) -> {:cont, x+a} end) 
  end



  
  
  def take(r, n) do
    case tk(r,n) do
     {:halted, taken} -> Enum.reverse(taken)
     {:done, {:sofar, _, taken}} -> Enum.reverse(taken)
   end
  end




  
  def tk(r,n) do
    reduce(r,
	  {:cont, {:sofar, 0, []}},
	 fn(x,{:sofar, s, a}) ->
	   s = s+1
	   if s >= n do
	     {:halt, [x|a]}
	   else
	     {:cont, {:sofar, s, [x|a]}}
	   end
      end)
  end
  
    

  def head(r) do
    reduce(r, {:cont, nil},
      fn (x, _) ->
	{:suspend, x}
      end)
  end

  def lazy_sum(r,a) do
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
    if from  <= to  do
      reduce({:range, from+1, to}, fun.(from, acc), fun)
    else
      {:done, acc}
    end
  end
      

end
