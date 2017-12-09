defmodule Waiting do

  def start(max) do
    {:ok, spwan_link(fn -> init(max) end)}
  end

  defp init(max), do: waiting(max, [])

  defp waiting(0, []) do
    IO.puts("Should not happen!")
    :ok
  end
  defp waiting(0, [next | rest] = queue) do
    receive do
      :close ->
        :ok
      {:enter, customer} ->
        send customer, :sorry
        waiting(0, queue)
      {:next, barber} ->
        send barber, {:ok, next}
        waiting(1, rest)
    end
  end
  defp waiting(n, []) do
    receive do
      :close ->
        :ok
      {:enter, customer} ->
        send customer, :please_wait
        waiting(n - 1, [customer])
    end
  end
  defp waiting(n, [next | rest] = queue) do
    receive do
      :close ->
        :ok
      {:next, barber} ->
        send barber, {:ok, next}
        waiting(n + 1, rest)
      {:enter, customer} ->
        send customer, :please_wait
        waiting(n - 1, queue ++ [customer])
    end
    
  end
end