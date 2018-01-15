defmodule Monitor do

  # Simple monitor
  def monitor(state) do
    receive do
      {:request, from} ->
        updated = critical(state)
        send(from, :ok)
        monitor(updated)
    end
  end

  # Monitor with lambda
  def monitor(state) do
    receive do
      {:request, fun, from} ->
        updated = fun.(state)
        send(from, :ok)
        monitor(updated)
    end
  end

end
