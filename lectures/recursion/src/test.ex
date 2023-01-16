defmodule Test do


  def test(x, y) do
    case foo(x, y) do 
      {:error, error} ->  
	:io.format("hmm, ~w/~w resulted in: ~s\n", [x,y, Exception.message(error)])
      {:ok, res} -> 
        :io.format("ok ~w/~w = ~w\n", [x,y,res])
    end
  end

  def foo(x, y) do
    try do
      {:ok, bar(x, y)}
    rescue
      error ->
        {:error, error}
    end
  end

  def bar(x,y) do
    x / y
  end
  
  
end
