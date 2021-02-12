defmodule Prgm do

  require Meta
  import Meta, only: [fun: 2]

  
  fun test() do 
    append({:a, {:b, :nil}}, {:c, {:d, :nil}})
  end

  fun append(xs, ys) do
    case xs do
      :nil -> ys
      {h, t} -> {h, append(t, ys)}
    end
  end

  fun nreverse(lst) do
    case lst do
      :nil -> :nil
      {h,t} -> append(nreverse(t), {h,:nil})
    end
  end

  fun map(lst, f) do
    case lst do
      :nil -> :nil
      {h, t} -> {f.(h), map(t, f)}
    end
  end
  

end

