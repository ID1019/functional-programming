defmodule Dull do

  def all_work(_cell, 0, jack) do 
    send(jack, :run)
  end
  def all_work(cell, n, jack) do
    x = Cell.read(cell)
    Cell.write(cell, x + 1)
    all_work(cell, n-1, jack)
  end

  def no_play(n) do
    cell = Cell.start(0)
    me = self()
    spawn(fn() -> 
       all_work(cell, n, me) end)
    spawn(fn() -> 
       all_work(cell, n, me) end)    
    receive do :run ->
        receive do :run ->
            Cell.read(cell)
        end
    end
  end
  

end

