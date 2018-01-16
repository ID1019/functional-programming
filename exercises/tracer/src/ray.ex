defmodule Ray do

  defstruct origin: {0, 0, 0}, direction: {1, 1, 1}

  def ray(origin, direction) do
    struct(Ray, origin: origin, direction: direction)
  end

  def vector(ray, length) do
    Vector.add(ray.origin, Vector.smul(ray.direction, length))
  end

end
