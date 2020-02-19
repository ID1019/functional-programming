defmodule Bench do

  def vanilla_read(n) do
    {t0, mem} = :timer.tc(fn() ->
      Mem.new(Enum.to_list(0..n))
    end)
    {t1, res} = :timer.tc(fn() ->
      values = Enum.map(0..n, fn(x) -> Mem.read(mem, x) end)
      List.foldl(values, 0, fn(v, a) -> v + a end)
    end)
    :io.format(" memory cretead in ~.2f ms~n", [t0/1000])
    :io.format(" operations performmed in ~.2f ms~n", [t1/1000])
    res    
  end

  def vanilla_write(n) do
    {t0, mem} = :timer.tc(fn() ->
      Mem.new(Enum.to_list(0..n))
    end)
    {t1, res} = :timer.tc(fn() ->
      Enum.reduce(0..n, mem, fn(x, m) -> Mem.write(m, x, :gurka) end)
      :ok
    end)
    :io.format(" memory cretead in ~.2f ms~n", [t0/1000])
    :io.format(" operations performmed in ~.2f ms~n", [t1/1000])
    res    
  end
  
  
  def synch(n) do
    {t0, mem} = :timer.tc(fn() ->
      Memory.new(Enum.to_list(0..n))
    end)
    {t1, res} = :timer.tc(fn() ->
      values = Enum.map(0..n, fn(x) -> Memory.read(mem, x) end)
      List.foldl(values, 0, fn(v, a) -> v + a end)
    end)
    :io.format(" memory cretead in ~.2f ms~n", [t0/1000])
    :io.format(" operations performmed in ~.2f ms~n", [t1/1000])
    res    
  end
  
  def asynch_read(n) do
    {t0, mem} = :timer.tc(fn() ->
      Redrum.new(Enum.to_list(0..n))
    end)
    {t1, res} = :timer.tc(fn() ->
      refs = Enum.map(0..n, fn(x) -> Redrum.read(mem, x) end)
      values = Enum.map(refs, fn(x) -> Redrum.collect(x) end)
      List.foldl(values, 0, fn(v, a) -> v + a end)
    end)
    :io.format(" memory cretead in ~.2f ms~n", [t0/1000])
    :io.format(" operations performmed in ~.2f ms~n", [t1/1000])
    res
  end

    def asynch_write(n) do
    {t0, mem} = :timer.tc(fn() ->
      Redrum.new(Enum.to_list(0..n))
    end)
    {t1, res} = :timer.tc(fn() ->
      Enum.map(0..n, fn(x) -> Redrum.write(mem, x, :gurka) end)
      :ok
    end)
    :io.format(" memory cretead in ~.2f ms~n", [t0/1000])
    :io.format(" operations performmed in ~.2f ms~n", [t1/1000])
    res
  end

  

  def synch(node, n) do
    {t0, mem} = :timer.tc(fn() ->
      Memory.remote(node, Enum.to_list(0..n))
    end)
    {t1, res} = :timer.tc(fn() ->
      values = Enum.map(0..n, fn(x) -> Memory.read(mem, x) end)
      List.foldl(values, 0, fn(v, a) -> v + a end)
    end)
    :io.format(" memory cretead in ~.2f ms~n", [t0/1000])
    :io.format(" operations performmed in ~.2f ms~n", [t1/1000])
    res
  end
  
  def asynch_read(node, n) do
    {t0, mem} = :timer.tc(fn() ->
      Redrum.remote(node, Enum.to_list(0..n))
    end)
    {t1, res} = :timer.tc(fn() ->
      refs = Enum.map(0..n, fn(x) -> Redrum.read(mem, x) end)
      values = Enum.map(refs, fn(x) -> Redrum.collect(x) end)      
      List.foldl(values, 0, fn(v, a) -> v + a end)
    end)
    :io.format(" memory cretead in ~.2f ms~n", [t0/1000])
    :io.format(" operations performed in  ~.2f ms~n", [t1/1000])        
    res
  end

  def asynch_write(node, n) do
    {t0, mem} = :timer.tc(fn() ->
      Redrum.remote(node, Enum.to_list(0..n))
    end)
    {t1, res} = :timer.tc(fn() ->
      Enum.map(0..n, fn(x) -> Redrum.write(mem, x, :gurka) end)
      :ok
    end)
    :io.format(" memory cretead in ~.2f ms~n", [t0/1000])
    :io.format(" operations performed in  ~.2f ms~n", [t1/1000])        
    res
  end
  

end
