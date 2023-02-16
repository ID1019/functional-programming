defmodule Split do

  defstruct [:next]

  def sequence(seq) do
    %Split{next: fn () ->  split(seq)  end}
  end
  
  defimpl Enumerable do

    def count(_) do  {:error, __MODULE__}  end
    def member?(_, _) do {:error, __MODULE__}  end
    def slice(_) do {:error, __MODULE__} end

    def reduce(_,    {:halt, acc}, _fun),   do: {:halted, acc}
    def reduce(seq,  {:suspend, acc}, fun), do: {:suspended, acc, fn(cmd) -> reduce(seq, cmd, fun) end}
    def reduce(%Split{next: f},  {:cont, acc}, fun) do
      case f.() do 
	{seq, f} ->
	  reduce(%Split{next: f}, fun.(seq, acc), fun)
	:done ->
	  {:done, acc}
      end
    end
  end

  def split([{1,x}|seq]) do 
    split(seq, x, [{1,x}], [], fn() -> :done end)
  end  
  def split([{n,x}|seq]) do 
    split([{n-1,x}|seq], x, [{1,x}], [], fn() -> :done end)
  end

  def split([], l, left, right, cont)  do
    {{left, right, l}, cont}
  end
  def split([{1, x}], l, [], right, cont)  do
    {{[{1,x}], right, l+x}, cont}
  end
  def split([{1,x}], l, left, [], cont)  do
    {{left, [{1,x}], l+x}, cont}
  end
  def split([{1,x}|rest], l, left, right, cont) do
    split(rest, l+x, add(left,{1,x}), right, fn() -> split(rest, l+x, left, add(right,{1,x}), cont) end)
  end
  def split(seq, l, left, right, cont) do
    split(0, seq, l, left, right, cont)
  end


  
  def split(n, [{n,x}], l, [], right, cont) do
    {{[{n,x}], right, l+(n*x)}, cont}
  end

  def split(n, [{n,x}], l, left, [], cont) do
    {{left, [{n,x}], l+(n*x)}, cont}
  end  
  def split(n, [{n,x}|rest], l, left, right, cont) do
    split(rest, l+(n*x), add(left, {n,x}), right, 
      fn() ->
	split(rest, l+(n*x), left, add(right, {n,x}), cont)
      end)
  end
  def split(i, [{n,x}|rest], l, left, right, cont) do
    split(rest, l+(n*x), add(left, {i,x}), add(right,{n-i,x}),
      fn() ->
	split(i+1, [{n,x}|rest], l, left, right, cont)
      end)
  end

  
  def add([],e) do [e] end
  def add([{_,h}|_]=seq,{_,x}=e) when h > x do [e|seq] end  
  def add([{n,x}|t],{i,x}) do [{n+i,x} | t] end
  def add([h|t],e) do [h| add(t,e)] end  
  
end
