defmodule Camera do

  require Record
  require Ray
  
  Record.defrecord(:camera, pos: nil, corner: nil, right: nil, down: nil, size: nil)

  def normal(size) do
    {width, height} = size
    d = width * 1.2
    h = width / 2
    v = height / 2
    pos = {0,0,0}
    corner = {-h, v, d}
    right = {1, 0, 0}
    down = {0, -1, 0}
    Camera.camera(pos: pos, corner: corner, right: right, down: down, size: size)
  end


  def ray(camera, x, y) do
    pos = Camera.camera(camera,:pos)
    x_pos = Vector.smul(Camera.camera(camera,:right), x)
    y_pos = Vector.smul(Camera.camera(camera,:down), y)
    pixle = Vector.add(Camera.camera(camera,:corner), Vector.add(x_pos, y_pos))
    dir = Vector.normalize(pixle)
    Ray.ray(pos: pos, dir: dir)
  end

end
