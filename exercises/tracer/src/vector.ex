defmodule Vector do

  # Scalar multiplication.
  def smul({x, y, z}, s) do
    {x * s, y * s, z * s}
  end

  # Addition and subtarction.
  def sub({x1, y1, z1}, {x2, y2, z2}) do
    {x1 - x2, y1 - y2, z1 - z2}
  end

  def add({x1, y1, z1}, {x2, y2, z2}) do
    {x1 + x2, y1 + y2, z1 + z2}
  end

  # Dot product.
  def dot({x1, y1, z1}, {x2, y2, z2}) do
    x1 * x2 + y1 * y2 + z1 * z2
  end

  # Scaling a vector to a specified length.
  def scale(x, l) do
    n = norm(x)
    # this will crash if N == 0!
    smul(x, l / n)
  end

  # Normalize by scaling to norm 1.
  def normalize(x) do
    scale(x, 1)
  end

  # Square of a vector.
  def sq(x) do
    dot(x, x)
  end

  # The norm (length) of a vector.
  def norm({x, y, z}) do
    :math.sqrt(x * x + y * y + z * z)
  end

  # Cross product, used to find a vector that as ortogonal to 
  # both x and y.
  def cross({x1, y1, z1}, {x2, y2, z2}) do
    {y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2}
  end

end
