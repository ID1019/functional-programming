defmodule Queue1 do

  def new() do
    {:queue, []}
  end
  
  def enqueue(itm, {:queue, itms}) do
    {:queue, last(itms, itm)}
  end

  def last([], itm) do [itm] end
  def last([head|tail], itm) do [head | last(tail, itm)] end
  
  def dequeue({:queue, itms}) do
    case itms do
      [itm | rest] ->
	{itm , {:queue, rest}}
    end
  end  

  def empty({:queue, []}) do true end    
  def empty({:queue, _}) do false end        


end
