defmodule Rainbow do

  def test() do
    img = Enum.map(1..960, fn(x) ->
      clr = Color.convert(x, 960)
      List.duplicate(clr, 540)
    end)
    PPM.write("rainbow.ppm", img)
  end



end

      
