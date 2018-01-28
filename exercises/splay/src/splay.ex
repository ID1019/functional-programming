defmodule Splay do

  def tree(), do: nil

  # In splay trees we will in each update operation move the key value
  # pair to the root of teh tree. We will try to keep the braches
  # equal in length but since the tree is ordered and the updated pair
  # should be in the root the tree will sometimes be unballanced. The
  # average tree will though be fairly well balanced. This will keep
  # operations in O(lg(n)) time komplexity. 

  # The root - we have two special cases and two general cases. The
  # general cases will use splay/2 to update a branch in the
  # tree. The result from updating the branch will change the upper
  # layer of the tree.

  def update(nil, key, value) do
    # The special case when the tree is empty.
    {:node, key, value, nil, nil}
  end
  def update({:node, key, _, a, b}, key, value) do
    # We've found what we're looking for and it is at the root.
    {:node, key, value, a, b}
  end
  def update({:node, rk, rv, zig, c}, key, value) when key < rk do
    # The general rule where we will do the Zig transformation.
    {:splay, _, a, b} = splay(zig, key)
    {:node, key, value, a, {:node, rk, rv, b, c}}
  end
  def update({:node, rk, rv, a, zag}, key, value) when key >= rk do
    # The general rule where we will do the Zag transformation.
    {:splay, _, b, c} = splay(zag, key)
    {:node, key, value, {:node, rk, rv, a, b}, c}
  end

  # This is the heart of the splay operation.  We will always return a
  # tuple {splay, Value, L, R} where L and R are subtrees. If The Key
  # is not found a 'na' value is returned.
  defp splay(nil, _) do
    # The special case when the tree is empty.
    {:splay, :na, nil, nil}
  end
  defp splay({:node, key, value, a, b}, key) do
    # We've found what we're looking for.
    {:splay, value, a, b}
  end
  defp splay({:node, rk, rv, nil, b}, key) when key < rk do
    # Should go left, but the left branch empty.
    {:splay, :na, nil, {:node, rk, rv, nil, b}}
  end
  defp splay({:node, rk, rv, a, nil}, key) when key >= rk do
    # Should go right, but the right branch empty.
    {:splay, :na, {:node, rk, rv, a, nil}, nil}
  end
  defp splay({:node, rk, rv, {:node, key, value, a, b}, c}, key) do
    # Found to the left.
    {:splay, value, a, {:node, rk, rv, b, c}}
  end
  defp splay({:node, rk, rv, a, {:node, key, value, b, c}}, key) do
    # Found to the right.
    {:splay, value, {:node, rk, rv, a, b}, c}
  end

  # Follows the general rules where we have the zig-zag patterns.
  defp splay({:node, gk, gv, {:node, pk, pv, zig_zig, c}, d}, key) when key < gk and key < pk do
    # Going down left-left, this is the so called zig-zig case. 
    {:splay, value, a, b} = splay(zig_zig, key)
    {:splay, value, a, {:node, pk, pv, b, {:node, gk, gv, c, d}}}
  end
  defp splay({:node, gk, gv, {:node, pk, pv, a, zig_zag}, d}, key) when key < gk and key >= pk do
    # Going down left-right, this is the so called zig-zag case. 
    {:splay, value, b, c} = splay(zig_zag, key)
    {:splay, value, {:node, pk, pv, a, b}, {:node, gk, gv, c, d}}
  end
defp splay({:node, gk, gv, a, {:node, pk, pv, zag_zig, d}}, key) when key >= gk and key < pk do
  # Going down right-left, this is the so called zag-zig case. 
  {:splay, value, b, c} = splay(zag_zig, key)
  {:splay, value, {:node, gk, gv, a, b}, {:node, pk, pv, c, d}}
end
defp splay({:node, gk, gv, a, {:node, pk, pv, b, zag_zag}}, key) when key >= gk and key >= pk do
  # Going down right-right, this is the so called zag-zag case. 
  {:splay, value, c, d} = splay(zag_zag, key)
  {:splay, value, {:node, pk, pv, {:node, gk, gv, a, b}, c}, d}
end

  # Same thing but now we will only do a lookup. The lookup will still
  # change the structure of the tree.

  #  The special case when the tree is empty.
  def lookup(nil, _), do: :fail
  def lookup({:node, key, value, a, b}, key) do
    # We've found what we're looking for and it is at the root.
    {:ok, value, {:node, key, value, a, b}}
  end
  def lookup({:node, rk, rv, z, c}, key) when key < rk do
    case splay(z, key) do
      {:splay, :na, _, _} ->
        :fail

      {:splay, value, a, b} ->
        {:ok, value, {:node, key, value, a, {:node, rk, rv, b, c}}}
    end
  end
  def lookup({:node, rk, rv, a, z}, key) when key >= rk do
    case splay(z, key) do
      {:splay, :na, _, _} ->
        :fail

      {:splay, value, b, c} ->
        {:ok, value, {:node, key, value, {:node, rk, rv, a, b}, c}}
    end
  end

  def test() do
    insert = [{3, :c}, {5, :e}, {2, :b}, {1, :a}, {7, :g}, {4, :d} {5, :e}]
    empty = nil
    List.foldl(insert, empty, fn({k, v}, t) -> update(t, k, v) end)
  end

end
