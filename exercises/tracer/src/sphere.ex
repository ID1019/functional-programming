defmodule Sphere do

  defstruct radius: 2, center: {0, 0, 0}

  def sphere(radius, center) do
    struct(Sphere, radius: radius, center: center)
  end

  def normal(i, sphere) do
    Vector.normalize(Vector.sub(i, sphere.center))
  end
  
end