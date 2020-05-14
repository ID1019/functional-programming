defmodule Out do

  def start() do
    spawn_link(fn() -> init() end)
  end

  def init() do
    out([])
  end

  def out(sofar) do
    receive do
       :done ->
	  done(Enum.reverse(sofar))
      {:alu, val} ->
	out([val|sofar])
    end
  end


  def done(collected) do
    receive do
      {:collect, client} ->
	send(client, {:out, collected])
	done(collected)
    end
  end
  

end
