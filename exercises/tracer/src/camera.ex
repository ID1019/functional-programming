defmodule Camera do

  defstruct(
    pos: nil,
    corner: nil,
    right: nil,
    down: nil,
    size: nil
  )

  def normal(size) do
    {width, height} = size
    d = width * 1.2
    h = width / 2
    v = height / 2
    pos = {0,0,0}
    corner = {-h, v, d}
    right = {1, 0, 0}
    down = {0, -1, 0}
    %Camera{pos: pos, corner: corner, right: right, down: down, size: size}
  end


  def ray(%Camera{pos: pos, right: right, down: down, corner: corner}, x, y) do
    x_pos = Vector.smul(right, x)
    y_pos = Vector.smul(down, y)
    pixle = Vector.add(corner, Vector.add(x_pos, y_pos))
    dir = Vector.normalize(pixle)
    %Ray{pos: pos, dir: dir}
  end

end
