defmodule Sphere do

  @color {1.0, 0.4, 0.4}
  @brilliance 0
  @transparency 0
  @refraction 1.5

  defstruct(
    radius: 2,
    pos: {0, 0, 0},
    color: @color,
    brilliance: @brilliance,
    transparency: @transparency,
    refraction: @refraction
  )
    

  defimpl Object do

    def intersect(sphere, ray) do
      Sphere.intersect(sphere, ray)
    end

    def normal(sphere, _, pos) do
      # assuming we always hit it from the outside
      Vector.normalize(Vector.sub(pos, sphere.pos))
    end

  end

  def intersect(%Sphere{pos: spos, radius: radius},  %Ray{pos: rpos, dir: dir}) do
    k = Vector.sub(spos, rpos)
    a = Vector.dot(dir, k)
    a2 = :math.pow(a, 2)
    k2 = :math.pow(Vector.norm(k), 2)
    r2 = :math.pow(radius, 2)
    t2 = a2 - k2 + r2
    closest(t2, a)
  end
    
  defp closest(t2, a) do
    if t2 < 0 do
      :no
    else
      t = :math.sqrt(t2)
      d1 = a - t
      d2 = a + t

      cond do
        d1 > 0.0 and d2 > 0.0 ->
          {:ok, min(d1, d2)}
        d1 > 0.0 ->
          {:ok, d1}
        d2 > 0.0 ->
          {:ok, d2}
        true ->
          :no
      end
    end
  end


  

  
end
