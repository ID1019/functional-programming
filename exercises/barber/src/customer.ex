defmodule Customer do
  
  @walk 1000

  # Start a customer process.
  def start(name, shop) do
    {:ok, spawn_link(fn -> init(name, shop) end)}
  end

  defp init(name, shop) do
    IO.puts("#{name} - going for a haircut")
    customer(name, shop)
  end

  # When a customer enters the shop three different
  # scenarios can happen:
  # 1) the barber is free and will give the haircut
  # 2) the barber is busy but there is space in the waiting
  #    queue; the customer is asked to wait
  # 3) the barber is busy and there is no space in the
  #    waiting queue; the customer is invited to take a
  #    walk and return later
  defp customer(name, shop) do
    IO.puts("#{name} - enter the shop")
    send(shop, {:hello, self()})

    receive do
      :please_wait ->
        IO.puts("#{name} - waiting")

        receive do
          :have_a_seat ->
            IO.puts("#{name} - taking a seat")

            receive do
              :howz_that ->
                IO.puts("#{name} - thank you")
            end
        end

      :sorry ->
        IO.puts("#{name} - taking a walk in the sun")
        :timer.sleep(@walk)
        customer(name, shop)
    end
  end
end
