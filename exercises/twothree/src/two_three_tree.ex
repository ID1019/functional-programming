defmodule TwoThreeTree do

  def test do
    insertf(14, :grk,
      {:two, 7, {:three, 2, 5, {:leaf, 2, :foo}, {:leaf, 5, :bar}, {:leaf, 7, :zot}},
      {:three, 13, 16, {:leaf, 13, :foo}, {:leaf, 16, :bar}, {:leaf, 18, :zot}}}
    )
  end

  def insertf(k, v, nil), do: {:leaf, k, v}
  def insertf(k, v, {:leaf, k1, _} = l) do
    cond do
      k <= k1 ->
        {:two, k, {:leaf, k, v}, l}

      true ->
        {:two, k1, l, {:leaf, k, v}}
    end
  end
  def insertf(k, v, {:two, k1, {:leaf, k1, _} = l1, {:leaf, k2, _} = l2}) do
    cond do
      k <= k1 ->
        {:three, k, k1, {:leaf, k, v}, l1, l2}

      k <= k2 ->
        {:three, k1, k, l1, {:leaf, k, v}, l2}

      true ->
        {:three, k1, k2, l1, l2, {:leaf, k, v}}
    end
  end
  def insertf(
        k,
        v,
        {:three, k1, k2, {:leaf, k1, _} = l1, {:leaf, k2, _} = l2, {:leaf, k3, _} = l3}
      ) do
    cond do
      k <= k1 ->
        {:four, k, k1, k2, {:leaf, k, v}, l1, l2, l3}

      k <= k2 ->
        {:four, k1, k, k2, l1, {:leaf, k, v}, l2, l3}

      k <= k3 ->
        {:four, k1, k2, k, l1, l2, {:leaf, k, v}, l3}

      true ->
        {:four, k1, k2, k3, l1, l2, l3, {:leaf, k, v}}
    end
  end
  def insertf(k, v, {:two, k1, left, right}) do
    cond do
      k <= k1 ->
        case insertf(k, v, left) do
          {:four, q1, q2, q3, t1, t2, t3, t4} ->
            {:three, q2, k1, {:two, q1, t1, t2}, {:two, q3, t3, t4}, right}

          updated ->
            {:two, k1, updated, right}
        end

      true ->
        case insertf(k, v, right) do
          {:four, q1, q2, q3, t1, t2, t3, t4} ->
            {:three, k1, q2, left, {:two, q1, t1, t2}, {:two, q3, t3, t4}}

          updated ->
            {:two, k1, left, updated}
        end
    end
  end
  def insertf(k, v, {:three, k1, k2, left, middle, right}) do
    cond do
      k <= k1 ->
        case insertf(k, v, left) do
          {:four, q1, q2, q3, t1, t2, t3, t4} ->
            {:four, q2, k1, k2, {:two, q1, t1, t2}, {:two, q3, t3, t4}, middle, right}

          updated ->
            {:three, k1, k2, updated, middle, right}
        end

      k <= k2 ->
        case insertf(k, v, middle) do
          {:four, q1, q2, q3, t1, t2, t3, t4} ->
            {:four, k1, q2, k2, left, {:two, q1, t1, t2}, {:two, q3, t3, t4}, right}

          updated ->
            {:three, k1, k2, left, updated, right}
        end

      true ->
        case insertf(k, v, right) do
          {:four, q1, q2, q3, t1, t2, t3, t4} ->
            {:four, k1, k2, q2, left, middle, {:two, q1, t1, t2}, {:two, q3, t3, t4}}

          updated ->
            {:three, k1, k2, left, middle, updated}
        end
    end
  end

  def debug(0), do: :ok
  def debug(n) do
    {path, t} = debug(1024, nil)
    d = depth(t)

    cond do
      d <= 10 ->
        debug(n - 1)

      true ->
        {d, path}
    end
  end

  def test(0, t), do: t
  def test(n, t) do
    test(n - 1, insert(Enum.random(0..1000), :foo, t))
  end

  def debug(0, t), do: {[], t}
  def debug(n, t) do
    r = Enum.random(0..1000)
    {path, tree} = debug(n - 1, insert(r, :foo, t))
    {[r | path], tree}
  end

  def path([], t), do: t
  def path([r | path], t) do
    path(path, insert(r, :foo, t))
  end

  def traverse({:leaf, k, _}), do: [k]
  def traverse({:two, _, l, r}) do
    traverse(l) ++ traverse(r)
  end
  def traverse({:three, _, _, l, m, r}) do
    traverse(l) ++ traverse(m) ++ traverse(r)
  end

  def depth({:leaf, _, _}), do: 0
  def depth({:two, _, l, r}) do
    1 + max(depth(l), depth(r))
  end
  def depth({:three, _, _, l, m, r}) do
    1 + max(depth(l), max(depth(m), depth(r)))
  end

  def insert(k, v, root) do
    case insertf(k, v, root) do
      {:four, q1, q2, q3, t1, t2, t3, t4} ->
        # Special case for the root of the tree, never want to end up with a four node.
        {:two, q2, {:two, q1, t1, t2}, {:two, q3, t3, t4}}

      updated ->
        updated
    end
  end
end
