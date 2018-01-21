defmodule Light do

  @white {1.0, 1.0, 1.0}

  defstruct pos: nil, color: @white

  def illuminate(obj, ill, world) do
    color = obj.color
    ambient = world.ambient
    ill(color, mul(ill, ambient))
  end

  def illuminate(obj, refl, ill, world) do
    color = obj.color
    bril = obj.brilliance
    ambient = world.ambient
    surface = ill(color, mul(ill, ambient))
    mul(surface, mod(refl, bril))
  end

  def illuminate(obj, refl, refr, ill, world) do
    color = obj.color
    bril = obj.brilliance
    transp = obj.transparency
    ambient = world.ambient
    surface = ill(color, mul(ill, ambient))
    mul(add(surface, refr, transp), mod(refl, bril))
  end

  def combine(point, normal, lights) do
    List.foldl(lights, {0, 0, 0}, fn light, contr ->
      mul(contribute(point, normal, light.pos, light.color), contr)
    end)
  end

  def contribute(point, normal, source, {r, g, b}) do
    direction = Vector.normalize(Vector.sub(source, point))
    cos = Vector.dot(direction, normal)
    {r * cos, g * cos, b * cos}
  end

  def mul({r1, g1, b1}, {r2, g2, b2}) do
    {1 - (1 - r1) * (1 - r2), 1 - (1 - g1) * (1 - g2), 1 - (1 - b1) * (1 - b2)}
  end

  def ill({r1, g1, b1}, {r2, g2, b2}) do
    {r1 * r2, g1 * g2, b1 * b2}
  end

  def mod({r1, g1, b1}, t) do
    {r1 * t, g1 * t, b1 * t}
  end

  def add({r1, g1, b1}, {r2, g2, b2}, t) do
    s = 1 - t
    {r1 * s + r2 * t, g1 * s + g2 * t, b1 * s + b2 * t}
  end

end
