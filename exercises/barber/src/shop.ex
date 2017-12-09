defmodule Shop do

  def start(max) do
    {:ok, spawn_link(fn -> init(max) end)}
  end

  defp init(max) do
    {:ok, waiting} = Waiting.start(max)
    {:ok, barber} = Barber.start(waiting)
    shop(waiting, barber)
  end

  defp shop(waiting, barber) do
    receive do
      {:hello, customer} ->
        send waiting, {:enter, customer}
        shop(waiting, barber)
      :close ->
        send waiting, :close
        send barber, :close
        :ok
    end
  end
end