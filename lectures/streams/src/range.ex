defmodule Range do



  def new(from, to) do {:range, from, to} end


  def map({:range, from, to}, f) do
    if (from <= to) do
      [f.(from) | map({:range, from+1, to}, f)]
    else
      []
    end
  end

  def foldl({:range, from, to}, acc, f) do

    if (from <= to) do
      foldl({:range, from+1, to}, f.(from, acc), f)
    else
      acc
    end

  end  

  def filter({:range, from, to}, acc, f) do
    if (from <= to) do
      if (f.(from) ) do 
        filter({:range, from+1, to}, [from|acc], f)
      else
        filter({:range, from+1, to}, acc, f)	
      end
    else
      acc
    end
  end
  

  def take({:range, from, to}, acc, n) do
    if (n > 0) do
      take({:range, from+1, to}, [from|acc], n-1)
    else
      acc
    end    
  end

  def take(r, n) do
    {:halted, taken} = reduce(r, {:cont, {:sofar, n, [] }},
      fn(x,{:sofar, n, a}) ->
        if n > 0 do
          {:cont, {:sofar, n-1, [x|a]}}
        else
          {:halt, [x|a]}
        end
      end)
    Enum.reverse(taken)
  end
  
  def reduce({:range, from , to}, {:cont, acc}, f) do
    if from <= to do
      reduce({:range, from+1, to}, f.(from, acc), f)
    else
      {:done, acc}
    end
  end  
  def reduce(range, {:suspend, acc}, f) do
    {:suspended, acc, fn(cmd) -> reduce(range, cmd, f) end}
  end
  def reduce(_, {:halt, acc}, _f) do
    {:halted, acc}
  end

  def head(range) do
    reduce(range,  {:cont, :nil}, 
      fn(x, _) ->
	{:suspend, x}
      end
    )
  end
  
      
  
  


  
end


