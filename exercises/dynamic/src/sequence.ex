defmodule Sequence do

  defstruct [:next]

  def sequence(seq) do
    %Sequence{next: fn () ->  split(seq)  end}
  end
  
  defimpl Enumerable do

    def count(_) do  {:error, __MODULE__}  end
    def member?(_, _) do {:error, __MODULE__}  end
    def slice(_) do {:error, __MODULE__} end

    def reduce(_,    {:halt, acc}, _fun),   do: {:halted, acc}
    def reduce(seq,  {:suspend, acc}, fun), do: {:suspended, acc, fn(cmd) -> reduce(seq, cmd, fun) end}
    def reduce(%Sequence{next: f},  {:cont, acc}, fun) do
      case f.() do 
	{seq, f} ->
	  reduce(%Sequence{next: f}, fun.(seq, acc), fun)
	:done ->
	  {:done, acc}
      end
    end
  end

  def split([x|seq]) do 
    split(seq, x, [x], [], fn() -> :done end)
  end

  def split([], l, left, right, cont)  do
    {{left, right, l}, cont}
  end
  def split([x], l, [], right, cont)  do
    {{[x], right, l+x}, cont}
  end
  def split([x], l, left, [], cont)  do
    {{left, [x], l+x}, cont}
  end
  def split([s|rest], l, left, right, cont) do
    split(rest, l+s, add(left,s), right, fn() -> split(rest, l+s, left, add(right,s), cont) end)
  end


  def add([],x) do [x] end
  def add([h|_]=seq,x) when h > x do [x|seq] end  
  def add([h|t],x) do [h | add(t,x)] end    
  
end
