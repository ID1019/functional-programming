defmodule TracerRefraction do

  @delta 0.001

  def tracer(camera, world) do
    {w, h} = camera.size
    xs = Enum.to_list(1..w)
    ys = Enum.to_list(1..h)
    for y <- ys, do: for(x <- xs, do: trace(x, y, camera, world))
  end

  defp trace(x, y, camera, world) do
    ray = Camera.ray(x, y, camera)
    depth = world.depth
    ref = world.refraction
    trace_ray(ray, 1, depth, ref, world)
  end

  defp trace_ray(_ray, _, 0, _, world) do
    world.background
  end
  defp trace_ray(_ray, cntr, _, _, world) when cntr < 0.02 do
    world.background
  end
  defp trace_ray(ray, cntr, depth, x1, world) do
    objects = world.objects

    case intersect(ray, objects) do
      {:inf, _} ->
        world.background

      {d, obj} ->
        o = ray.origin
        l = ray.direction
        i1 = Vector.add(o, Vector.smul(l, d - @delta))
        normal = Sphere.normal(i1, obj)
        visible = visible(i1, world.lights, objects)
        illumination = Light.combine(i1, normal, visible)
        r1 = Ray.ray(i1, reflection(l, normal))
        reflection = trace_ray(r1, cntr * obj.brilliance, depth - 1, x1, world)
        x2 = obj.refraction

        case refraction(l, x1, x2, normal) do
          :na ->
            Light.illuminate(obj, reflection, illumination, world)

          refr ->
            i2 = Vector.add(o, Vector.smul(l, d + @delta))
            r2 = Ray.ray(i2, refr)
            refraction = trace_ray(r2, cntr * obj.brilliance, depth - 1, x2, world)
            Light.illuminate(obj, reflection, refraction, illumination, world)
        end
    end
  end

  defp intersect(ray, objects) do
    List.foldl(objects, {:inf, :no}, fn(object, sofar) ->
      {dist, _} = sofar

      case Objects.intersect(object, ray) do
        {:ok, d} when d < dist ->
          {d, object}

        _ ->
          sofar
      end
    end)
  end

  defp visible(point, lights, objs) do
    Enum.filter(lights, fn light -> clear(point, light.origin, objs) end)
  end

  defp clear(point, origin, objs) do
    dir = Vector.normalize(Vector.sub(origin, point))

    List.foldl(objs, true, fn(obj, acc) ->
      case acc do
        false ->
          false

        true ->
          case Objects.intersect(obj, Ray.ray(point, dir)) do
            :no ->
              true

            _ ->
              false
          end
      end
    end)
  end

  defp reflection(l, n) do
    Vector.sub(l, Vector.smul(n, 2 * Vector.dot(l, n)))
  end

  defp refraction(l, i, j, n) do
    r = i / j
    c = -Vector.dot(n, l)
    q = 1 - :math.pow(r, 2) * (1 - :math.pow(c, 2))

    cond do
      q < 0 -> :na
      true -> Vector.add(Vector.smul(l, r), Vector.smul(n, r * c - :math.sqrt(q)))
    end
  end

end
