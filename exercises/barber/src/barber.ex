defmodule Barber do
  
  @cut 1000

  def start(waiting) do
    {:ok, spawn_link(fn -> init(waiting) end)}
  end

  defp init(waiting), do: barber(waiting)
  
  defp barber(waiting) do
    send waiting, {:next, self()}
    receive do
      :close ->
        :ok
      {:ok, customer} ->
        send customer, :have_a_seat
        :timer.sleep(@cut)
        send customer, :howz_that
        barber(waiting)
    end
  end
  
end