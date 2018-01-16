defmodule Camera do

  defstruct pos: nil, corner: nil, right: nil, down: nil, size: nil

  def camera(pos, corner, right, down, size) do
    struct(Camera, pos: pos, corner: corner, right: right, down: down, size: size)
  end

  def normal(size) do
    {width, height} = size
    d = width * 1.2
    h = width / 2
    v = height / 2
    corner = {-h, v, d}
    camera({0, 0, 0}, corner, {1, 0, 0}, {0, -1, 0}, size)
  end

  def size(camera), do: camera.size

  def ray(x, y, camera) do
    origin = camera.pos
    x_pos = Vector.smul(camera.right, x)
    y_pos = Vector.smul(camera.down, y)
    pixle = Vector.add(camera.corner, Vector.add(x_pos, y_pos))
    dir = Vector.normalize(pixle)
    Ray.ray(origin, dir)
  end

end
