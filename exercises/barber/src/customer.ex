defmodule Customer do

  @walk 1000

  def start(name, shop) do
    {:ok, spawn_link(fn -> init(name, shop) end)}
  end

  defp init(name, shop) do
    IO.puts("#{name} - going for a haircut")
    customer(name, shop)
  end

  defp customer(name, shop) do
    IO.puts("#{name} - enter the shop")
    send shop, {:hello, self()}
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