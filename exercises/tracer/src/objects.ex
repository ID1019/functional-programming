defmodule Objects do

  def intersect(sphere, ray) do
    k = Vector.sub(sphere.center, ray.origin)
    a = Vector.dot(ray.direction, k)
    a2 = :math.pow(a, 2)
    k2 = :math.pow(Vector.norm(k), 2)
    r2 = :math.pow(sphere.radius, 2)
    t2 = a2 - k2 + r2
    closest(t2, a)
  end

  defp closest(t2, a) do
    cond do
      t2 < 0 ->
        :no

      true ->
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
