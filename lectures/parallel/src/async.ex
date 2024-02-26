defmodule Async do

  def eval(lambda) do
    me = self()
    ref = make_ref()
    spawn_link(fn() -> send(me, {:async, ref, lambda.()}) end)
    ref
  end

  def collect(ref) do
    receive do
      {:async, ^ref, res} ->
	res
    end
  end
  
  

end
