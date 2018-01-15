defmodule Waiting do
  
  def start(max) do
    {:ok, spawn_link(fn -> init(max) end)}
  end

  defp init(max), do: waiting(max, [])

  # Manages the waiting system of the barber shop including
  # the waiting times and the logic of customers entering
  # the shop and the barber's work. Gently stop the process
  # when the shop closes.
  defp waiting(0, []) do
    IO.puts("Should not happen!")
    :ok
  end
  defp waiting(0, [next | rest] = queue) do
    receive do
      :close ->
        :ok

      {:enter, customer} ->
        send(customer, :sorry)
        waiting(0, queue)

      {:next, barber} ->
        send(barber, {:ok, next})
        waiting(1, rest)
    end
  end
  defp waiting(n, []) do
    receive do
      :close ->
        :ok

      {:enter, customer} ->
        send(customer, :please_wait)
        waiting(n - 1, [customer])
    end
  end
  defp waiting(n, [next | rest] = queue) do
    receive do
      :close ->
        :ok

      {:next, barber} ->
        send(barber, {:ok, next})
        waiting(n + 1, rest)

      {:enter, customer} ->
        send(customer, :please_wait)
        waiting(n - 1, queue ++ [customer])
    end
  end
end
