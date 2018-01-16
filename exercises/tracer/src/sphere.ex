defmodule Sphere do

  @color {1.0, 0.4, 0.4}
  @brilliance 0
  @transparency 0
  @refraction 1.5

  defstruct radius: 2,
            center: {0, 0, 0},
            color: @color,
            brilliance: @brilliance,
            transparency: @transparency,
            refraction: @refraction

  def sphere(radius, center) do
    struct(Sphere, radius: radius, center: center)
  end

  def sphere(radius, center, opt) do
    color =
      case List.keyfind(opt, :color, 0) do
        {:color, c} -> c
        nil -> @color
      end

    brilliance =
      case List.keyfind(opt, :brilliance, 0) do
        {:brilliance, b} -> b
        nil -> @brilliance
      end

    transparency =
      case List.keyfind(opt, :transparency, 0) do
        {:transparency, t} -> t
        nil -> @transparency
      end

    refraction =
      case List.keyfind(opt, :refraction, 0) do
        {:refraction, r} -> r
        nil -> @refraction
      end

    struct(
      Sphere,
      radius: radius,
      center: center,
      color: color,
      brilliance: brilliance,
      transparency: transparency,
      refraction: refraction
    )
  end

  def normal(i, sphere) do
    Vector.normalize(Vector.sub(i, sphere.center))
  end

end
