defmodule Bench do


  def test1(n) do
    Enum.reduce(1..n, "1234567890", fn(n, acc) ->
      {t, acc} = :timer.tc(fn() -> acc <> acc end)
      :io.format(" ~w : ~w\n", [n, t])
      acc
    end)
    :ok
  end

  def test2(n, k) do
    str = sample(k)
    Enum.reduce(1..n, "", fn(n, acc) ->
      {t, acc} = :timer.tc(fn() -> str <> acc end)
      :io.format("  ~w : ~w\n", [n, t])
      acc
    end)
    :ok
  end

  def test3(n, k) do
    str = sample(k)
    Enum.reduce(1..n, "", fn(n, acc) ->
      ##  we could be lucky and add the string to the end with constant cost
      {t, acc} = :timer.tc(fn() -> acc <> " " <> str  end)
      :io.format(" ~w : ~w\n", [n, t])
      acc
    end)
    :ok
  end

  def test4(n, k) do
    str = sample(k)
    Enum.reduce(1..n, "", fn(n, acc) ->
      {t, {_, acc}} = :timer.tc(fn() ->
         {acc <> "123", acc <> str}
    end)
      :io.format(" ~w : ~w\n", [n, t])
      acc
    end)
    :ok
  end
  

  def sample(k) do
    List.to_string(List.duplicate(?a,k))
  end
  

  
  

end
