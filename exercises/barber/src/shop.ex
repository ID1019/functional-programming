defmodule Shop do
  
  # Open the barber shop.
  def start(max) do
    {:ok, spawn_link(fn -> init(max) end)}
  end

  # Setup the shop with an allowed waiting list of 'max'
  # number of places. Start the barber waiting for customers.
  defp init(max) do
    {:ok, waiting} = Waiting.start(max)
    {:ok, barber} = Barber.start(waiting)
    shop(waiting, barber)
  end

  # Handle a new customer who enters the shop (:hello) and
  # a close message (:close).
  defp shop(waiting, barber) do
    receive do
      {:hello, customer} ->
        send(waiting, {:enter, customer})
        shop(waiting, barber)

      :close ->
        send(waiting, :close)
        send(barber, :close)
        :ok
    end
  end
end
