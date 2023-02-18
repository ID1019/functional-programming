defmodule Range do

  def new(from, to) do {:range, from, to} end

  def sum(range) do reduce(range, {:cont, 0}, fn(x,acc) -> {:cont, x + acc} end) end

  def map(range, f) do 
    reduce(range, {:cont, []}, fn(x, acc) -> {:cont, [f.(x) | acc]} end)
  end

  def filter(range, f) do reduce(range, {:cont, []},
	  fn(x, acc) ->
	    if (f.(x) ) do
	      {:cont, [x | acc]}
	    else
	      {:cont, acc}
	    end
	  end)
  end

  def take(range, n) do reduce(range, {:cont, {:sofar, n, []}},
	  fn( x, {:sofar, n, acc}) ->
	    if ( n > 0 ) do
	      {:cont, {:sofar, n-1, [x|acc]}}
	    else
	      {:halt,  acc}
	    end
	  end)
  end

  def head(range) do
    reduce(range, {:cont, :nil}, fn (x, _) -> {:suspend, x} end)
  end
  

  def reduce({:range, from, to}, {:cont, acc}, f) do
    if (from<= to) do
      reduce({:range, from+1, to}, f.(from,acc), f)
    else
      {:done, acc}
    end
  end
  def reduce(range, {:suspend, acc}, f) do
    {:suspended, acc, fn(cmd) -> reduce(range, cmd, f) end}
  end
  def reduce(_, {:halt, acc}, _) do  
    {:halted, acc}
  end


  
  def foldl({:range, from, to}, acc, f) do
    if (from<= to) do
      foldl({:range, from+1, to}, f.(from,acc), f)
    else
      acc
    end
  end



  

  
  
end


