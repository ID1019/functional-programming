defmodule Out do

  def start() do
    spawn_link(fn() -> init() end)
  end

  def init() do
    out([])
  end

  def wait(out) do
    ref = make_ref()
    send(out, {:collect, ref, self()})
    receive do
      {:out, ^ref, collected} ->
	collected
    end
  end

  def out(sofar) do
    receive do
      {:alu, :done} ->
	done(Enum.reverse(sofar))
      {:alu, val} ->
	out([val|sofar])
    end
  end

  def done(collected) do
    receive do
      {:collect, ref, client} ->
	send(client, {:out, ref, collected})
	:ok
    end
  end

end

