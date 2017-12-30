defmodule Light do

  defstruct origin: nil, color: {1.0, 1.0, 1.0}

  def light(origin, color) do
    struct(Light, origin: origin, color: color)
  end

  def origin(light), do: light.origin
    
  def illuminate(sphere, ill, world) do
    color = Sphere.color(sphere)
    ambient = World.ambient(world)
    ill(color, mul(ill, ambient))
  end

  def combine(point, normal, lights) do
    List.foldl(lights, {0, 0, 0}, fn(light, contr) ->
        mul(contribute(point, normal, light.origin, light.color), contr)
      end)
  end

  def contribute(point, normal, source, {r, g, b}) do
    direction = Vector.normalize(Vector.sub(source, point))
    cos = Vector.dot(direction, normal)
    {r*cos, g*cos, b*cos}
  end

  def mul({r1, g1, b1}, {r2, g2, b2}) do
    {(1 - ((1-r1) *(1-r2))), (1 - ((1-g1)*(1-g2))), (1 - ((1-b1)*(1-b2)))}
  end

  def ill({r1, g1, b1}, {r2, g2, b2}) do
    {r1*r2, g1*g2, b1*b2}
  end

end