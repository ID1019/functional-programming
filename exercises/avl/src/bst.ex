defmodule Bst do

  def tree(), do: nil

  def insert(nil, key, value) do
    {:node, key, value, nil, nil}
  end
  def insert({:node, key, _, a, b}, key, value) do
    {:node, key, value, a, b}
  end
  def insert({:node, rk, rv, a, b}, kk, kv) when kk < rk do
    {:node, rk, rv, insert(a, kk, kv), b}
  end
  def insert({:node, rk, rv, a, b}, kk, kv) when kk > rk do
    {:node, rk, rv, a, insert(b, kk, kv)}
  end

  def lookup(nil, _), do: :fail
  def lookup({:node, k, v, _, _}, k), do: {:ok, v}
  def lookup({:node, k, _, l, r}, key) do
    cond do
      key < k -> lookup(l, key)
      true -> lookup(r, key)
    end
  end

  def traverse(nil), do: {0, 0}
  def traverse({:node, _, _, l, r}) do
    {lt, lm} = traverse(l)
    {rt, rm} = traverse(r)    
    {lt + rt + 1, max(lm, rm) + 1}
  end

  def depth(nil, _), do: :fail
  def depth({:node, k, _, _, _}, k), do: {:ok, 1}
  def depth({:node, k, _, l, r}, key) do
    cond do
      key < k ->
        case depth(l, key) do
          {:ok, d} -> {:ok, d + 1}
          :fail -> :fail
        end
      true ->
        case depth(r, key) do
          {:ok, d} -> {:ok, d + 1}
          :fail -> :fail
        end
    end
  end

  def max_depth(nil), do: 0
  def max_depth({:node, _, _, l, r}) do
    max(max_depth(l), max_depth(r)) + 1
  end
    
end