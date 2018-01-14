defmodule Barber do
  
  # The time needed for an haircut.
  @cut 1000

  # Start a barber process.
  def start(waiting) do
    {:ok, spawn_link(fn -> init(waiting) end)}
  end

  defp init(waiting), do: barber(waiting)

  # If the barber is free it notifies the waiting system
  # to send a new customer. Then invites the customer to
  # have a seat and gives the haircut. Gently stop the
  # process when the shop closes.
  defp barber(waiting) do
    send(waiting, {:next, self()})

    receive do
      :close ->
        :ok

      {:ok, customer} ->
        send(customer, :have_a_seat)
        :timer.sleep(@cut)
        send(customer, :howz_that)
        barber(waiting)
    end
  end
end
