defmodule Light do

  @white {1.0, 1.0, 1.0}

  defstruct(
    pos: nil,
    color: @white
  )

  def illuminate(obj, ill, world) do
    color = obj.color
    ill(color, mul(ill, world.ambient))
  end

  def illuminate(obj, refl, ill, world) do
    surface = ill(obj.color, mul(ill, world.ambient))
    mul(surface, mod(refl, obj.brilliance))
  end

  def illuminate(obj, refl, refr, ill, world) do
    surface = ill(obj.color, mul(ill, world.ambient))
    mul(add(surface, refr, obj.transparency), mod(refl, obj.brilliance))
  end

  def combine(point, normal, lights) do
    List.foldl(lights, {0, 0, 0},
      fn(light, contr) ->
         mul(contribute(point, normal, light.pos, light.color), contr)
      end)
  end

  def contribute(point, normal, source, {r, g, b}) do
    direction = Vector.normalize(Vector.sub(source, point))
    cos = Vector.dot(direction, normal)
    {r * cos, g * cos, b * cos}
  end


  ## How to work with colors

  ## combine the two lights 
  def mul({r1, g1, b1}, {r2, g2, b2}) do
    {1 - (1 - r1) * (1 - r2), 1 - (1 - g1) * (1 - g2), 1 - (1 - b1) * (1 - b2)}
  end

  ## illuminate a surface of a given color
  def ill({r1, g1, b1}, {r2, g2, b2}) do
    {r1 * r2, g1 * g2, b1 * b2}
  end

  ## reduce a light 
  def mod({r1, g1, b1}, t) do
    {r1 * t, g1 * t, b1 * t}
  end

  ## combine two colors given a ratio
  def add({r1, g1, b1}, {r2, g2, b2}, t) do
    s = 1 - t
    {r1 * s + r2 * t, g1 * s + g2 * t, b1 * s + b2 * t}
  end

end
