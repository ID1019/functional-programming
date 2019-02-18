defmodule Cell do

  def start(val) do       ## things to do in mother process
    {:cell, spawn_link(fn() -> init(val) end)}
  end

  def remote(node, val) do
    {:cell, Node.spawn_link(node, fn() -> init(val) end)}
  end
  
  
  def read({:cell, cell}) do
    send(cell , {:read, self()})
    receive do
      {:value, v} ->
	v
    end
  end

  def write({:cell, cell}, val) do
    send(cell, {:write, val, self()})
    receive do
    :ok ->
      :ok
    end
  end

  def quit({:cell, cell}) do
    send(cell, :quit)
  end  
  
  def init(val) do        ## things to do in the child process
    cell(val)
  end

  def cell(v) do
    receive do
      {:read, pid} ->
	send(pid, {:value, v})
	cell(v)
      {:write, w, pid} ->
	send(pid, :ok)
	cell(w)
      :quit ->
	:ok
    end
  end

end
  
