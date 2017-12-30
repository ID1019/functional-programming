defmodule Ray do

  defstruct origin: {0, 0, 0}, direction: {1, 1, 1}

  def ray(origin, direction) do
    struct(Ray, origin: origin, direction: direction)
  end

  def origin(ray), do: ray.origin

  def direction(ray), do: ray.direction

end