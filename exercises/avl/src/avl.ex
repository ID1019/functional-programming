defmodule AVL do

  # ================================================== #
  # == AVL Tree - Public API == #
  # ================================================== #

  # Create an empty tree.
  def tree(), do: nil

  # Insert a key-value pair in a tree.
  # Usage: insert(tree, key, value)
  def insert(tree, key, value) do
    case insrt(tree, key, value) do
      {:inc, q} -> q
      {:ok, q} -> q
    end
  end

  # Lookup for a given key in a tree.
  # Usage: lookup(tree, key)
  def lookup(nil, _), do: :fail
  def lookup({:node, k, v, _, _, _}, k), do: {:ok, v}
  def lookup({:node, k, _, _, l, r}, key) do
    cond do
      key < k -> lookup(l, key)
      true -> lookup(r, key)
    end
  end

  # Compute the depth of a the leaf containing a given key
  # in a tree.
  # Usage: depth(tree, key)
  def depth(nil, _), do: :fail
  def depth({:node, k, _, _, _, _}, k), do: {:ok, 1}
  def depth({:node, k, _, _, l, r}, key) do
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

  # Compute the maximum depth of a tree.
  # Usage: max_depth(tree)
  def max_depth(nil), do: 0
  def max_depth({:node, _, _, _, l, r}) do
    max(max_depth(l), max_depth(r)) + 1
  end

  # Traverse the tree to compute a tuple containing the 
  # number of nodes and the maximum depth.
  # Usage: traverse(tree)
  def traverse(nil), do: {0, 0}
  def traverse({:node, _, _, _, l, r}) do
    {lt, lm} = traverse(l)
    {rt, rm} = traverse(r)
    {lt + rt + 1, max(lm, rm) + 1}
  end

  # ================================================== #
  # == Private methods == #
  # ================================================== #

  defp insrt(nil, key, value) do
    {:inc, {:node, key, value, 0, nil, nil}}
  end
  defp insrt({:node, key, _, f, a, b}, key, value) do
    {:ok, {:node, key, value, f, a, b}}
  end
  defp insrt({:node, rk, rv, 0, a, b}, kk, kv) when kk < rk do
    case insrt(a, kk, kv) do
      {:inc, q} ->
        {:inc, {:node, rk, rv, -1, q, b}}

      {:ok, q} ->
        {:ok, {:node, rk, rv, 0, q, b}}
    end
  end
  defp insrt({:node, rk, rv, 0, a, b}, kk, kv) when kk > rk do
    case insrt(b, kk, kv) do
      {:inc, q} ->
        {:inc, {:node, rk, rv, +1, a, q}}

      {:ok, q} ->
        {:ok, {:node, rk, rv, 0, a, q}}
    end
  end
  defp insrt({:node, rk, rv, +1, a, b}, kk, kv) when kk < rk do
    case insrt(a, kk, kv) do
      {:inc, q} ->
        {:ok, {:node, rk, rv, 0, q, b}}

      {:ok, q} ->
        {:ok, {:node, rk, rv, +1, q, b}}
    end
  end
  defp insrt({:node, rk, rv, -1, a, b}, kk, kv) when kk > rk do
    case insrt(b, kk, kv) do
      {:inc, q} ->
        {:ok, {:node, rk, rv, 0, a, q}}

      {:ok, q} ->
        {:ok, {:node, rk, rv, -1, a, q}}
    end
  end
  defp insrt({:node, rk, rv, -1, a, b}, kk, kv) when kk < rk do
    case insrt(a, kk, kv) do
      {:inc, q} ->
        {:ok, rotate({:node, rk, rv, -2, q, b})}

      {:ok, q} ->
        {:ok, {:node, rk, rv, -1, q, b}}
    end
  end
  defp insrt({:node, rk, rv, +1, a, b}, kk, kv) when kk > rk do
    case insrt(b, kk, kv) do
      {:inc, q} ->
        {:ok, rotate({:node, rk, rv, +2, a, q})}

      {:ok, q} ->
        {:ok, {:node, rk, rv, +1, a, q}}
    end
  end

  # Single rotate left
  defp rotate({:node, xk, xv, -2, {:node, yk, yv, -1, a, b}, c}) do
    {:node, yk, yv, 0, a, {:node, xk, xv, 0, b, c}}
  end

  # Single rotate right
  defp rotate({:node, xk, xv, +2, a, {:node, yk, yv, +1, b, c}}) do
    {:node, yk, yv, 0, {:node, xk, xv, 0, a, b}, c}
  end

  # Double rotate left
  defp rotate({:node, xk, xv, -2, {:node, yk, yv, +1, a, {:node, zk, zv, 0, b, c}}, d}) do
    {:node, zk, zv, 0, {:node, yk, yv, 0, a, b}, {:node, xk, xv, 0, c, d}}
  end
  defp rotate({:node, xk, xv, -2, {:node, yk, yv, +1, a, {:node, zk, zv, -1, b, c}}, d}) do
    {:node, zk, zv, 0, {:node, yk, yv, 0, a, b}, {:node, xk, xv, +1, c, d}}
  end
  defp rotate({:node, xk, xv, -2, {:node, yk, yv, +1, a, {:node, zk, zv, +1, b, c}}, d}) do
    {:node, zk, zv, 0, {:node, yk, yv, -1, a, b}, {:node, xk, xv, 0, c, d}}
  end

  # Double rotate right
  defp rotate({:node, xk, xv, +2, a, {:node, yk, yv, -1, {:node, zk, zv, 0, b, c}, d}}) do
    {:node, zk, zv, 0, {:node, xk, xv, 0, a, b}, {:node, yk, yv, 0, c, d}}
  end
  defp rotate({:node, xk, xv, +2, a, {:node, yk, yv, -1, {:node, zk, zv, +1, b, c}, d}}) do
    {:node, zk, zv, 0, {:node, xk, xv, -1, a, b}, {:node, yk, yv, 0, c, d}}
  end
  defp rotate({:node, xk, xv, +2, a, {:node, yk, yv, -1, {:node, zk, zv, -1, b, c}, d}}) do
    {:node, zk, zv, 0, {:node, xk, xv, 0, a, b}, {:node, yk, yv, +1, c, d}}
  end
end
