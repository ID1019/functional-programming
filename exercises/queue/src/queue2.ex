defmodule Queue2 do

  def new() do
    {:queue, [], []}
  end
  
  def enqueue(itm, {:queue, front, back}) do
    {:queue, front, [itm|back]}
  end

  def dequeue({:queue, [], back}) do
    case Enum.reverse(back) do
      [itm|rest] ->
	{itm, {:queue, rest, []}}
    end
  end
  def dequeue({:queue, [itm|rest], back}) do
    {itm , {:queue, rest, back}}
  end  

  def empty({:queue, [], []}) do true end    
  def empty({:queue, _, _}) do false end        


end
