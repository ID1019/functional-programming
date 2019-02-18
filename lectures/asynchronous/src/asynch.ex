defmodule Asynch do

  def start(val) do       ## things to do in mother process
    {:asynch, spawn_link(fn() -> init(val) end)}
  end

  def remote(node, val) do
    {:asynch, Node.spawn_link(node, fn() -> init(val) end)}
  end
    
  def read({:asynch, cell}) do
    ref = make_ref()
    send(cell , {:read, ref, self()})
    ref
  end

  def write({:asynch, cell}, val) do
    send(cell, {:write, val})
  end

  def write_synch({:asynch, cell}, val) do
    ref = make_ref()
    send(cell, {:write, val, ref, self()})
    receive do
      {:ok, ^ref} ->
	:ok
    end
  end

  def quit({:asynch, cell}) do
    send(cell, :quit)
  end
  

  def init(val) do        ## things to do in the child process
    cell(val)
  end

  def cell(v) do
    receive do
      {:read, ref, pid} ->
	send(pid, {:value, ref, v})
	cell(v)
      {:write, w} ->
	cell(w)
      {:write, w, ref, pid} ->
	send(pid, {:ok, ref})
	cell(w)
      :quit ->
	:ok
    end
  end

end
  
