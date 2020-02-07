defmodule TracerLight do

  @delta 0.001


  def tracer(camera, world) do
    {w, h} = camera.size
    for y <- 1..h, do: for(x <- 1..w, do: trace(x, y, camera, world))
  end

  defp trace(x, y, camera, world) do
    ray = Camera.ray(camera, x, y)
    trace(ray, world)
  end
  defp trace(ray, %World{objects: objects}=world) do

    case intersect(ray, objects) do
      {:inf, _} ->
        world.background

      {d, obj} ->
	i = Vector.add(ray.pos, Vector.smul(ray.dir, d - @delta))
        normal = Object.normal(obj,ray,i)
        visible = visible(i, world.lights, objects)
        illumination = Light.combine(i, normal, visible)
        Light.illuminate(obj, illumination, world)
    end
  end

  defp intersect(ray, objects) do
    List.foldl(objects, {:inf, nil}, fn (object, sofar) ->
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
    Enum.filter(lights, fn light -> clear(point, light.pos, objs) end)
  end

  defp clear(point, origin, objs) do
    dir = Vector.normalize(Vector.sub(origin, point))

    List.foldl(objs, true, fn (obj, acc) ->
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

end
