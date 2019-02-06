defmodule Tmp do


  def member(l, x) do
    case l do
      [] -> :no
      [^x|_] -> :yes
      [_|t] -> member(t, x)
    end
  end

  

  def test(x) do
    case x do
      :hello -> z = 2
      :goodby ->  z = 3
    end
    z
  end


  def loop() do loop() end

  def ops(n) do 4/n end

end
