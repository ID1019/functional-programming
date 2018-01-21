defmodule TracerReflection do

  @delta 0.001

  def tracer(camera = %Camera{}, world = %World{}) do
    {w, h} = camera.size
    xs = Enum.to_list(1..w)
    ys = Enum.to_list(1..h)
    for y <- ys, do: for(x <- xs, do: trace(x, y, camera, world))
  end

  defp trace(x, y, camera, world) do
    ray = Camera.ray(camera, x, y)
    depth = world.depth
    trace(ray, depth, world)
  end
  defp trace(_ray, 0, world) do
    world.background
  end
  defp trace(ray, depth, world) do
    objects = world.objects

    case intersect(ray, objects) do
      {:inf, _} ->
        world.background

      {d, obj} ->
        o = ray.pos
        l = ray.dir
        i = Vector.add(o, Vector.smul(l, d - @delta))
        normal = Object.normal(obj, i)
        visible = visible(i, world.lights, objects)
        illumination = Light.combine(i, normal, visible)
        r = %Ray{pos: i, dir: reflection(l, normal)}
        reflection = trace(r, depth - 1, world)
        Light.illuminate(obj, reflection, illumination, world)
    end
  end

  defp intersect(ray, objects) do
    List.foldl(objects, {:inf, nil}, fn(object, sofar) ->
      {dist, _} = sofar

      case Object.intersect(object, ray) do
        {:ok, d} when d < dist ->
          {d, object}

        _ ->
          sofar
      end
    end)
  end

  defp visible(point, lights, objs) do
    Enum.filter(lights, fn(light) -> clear(point, light.pos, objs) end)
  end

  defp clear(point, origin, objs) do
    dir = Vector.normalize(Vector.sub(origin, point))

    List.foldl(objs, true, fn(obj, acc) ->
      case acc do
        false ->
          false

        true ->
          case Object.intersect(obj, %Ray{pos: point, dir: dir}) do
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

end
