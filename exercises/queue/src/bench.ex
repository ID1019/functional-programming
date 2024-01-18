defmodule Bench do

  def time(module, n) do
    {t, _} = :timer.tc( fn() -> test(module, n) end)
    t
  end
  
  
  def test(module, n) do
    q = module.new()
    q = enqueue(module, q, n)
    dequeue(module, q)
  end

  def enqueue(module, q, n) do
    if n == 0 do
      q
    else
      enqueue(module, module.enqueue(n, q), n-1)
    end
  end
  
  def dequeue(module, queue) do
    if module.empty(queue) do
      :ok
    else
      {_, rest} = module.dequeue(queue)
      dequeue(module, rest)
    end
  end
  


end
