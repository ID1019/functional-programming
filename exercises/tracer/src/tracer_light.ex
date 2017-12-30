defmodule TracerLight do

  @delta 0.001

  def tracer(camera, world) do
    {w, h} = Camera.size(camera)
    xs = Enum.to_list(1..w)
    ys = Enum.to_list(1..h)
    for y <- ys, do: (for x <- xs, do: trace(x, y, camera, world))
  end

  defp trace(x, y, camera, world) do
    ray = Camera.ray(x, y, camera)
    trace(ray, world)
  end

  defp trace(ray, world) do
    objects = World.objects(world) 
    case intersect(ray, objects) do
      {:inf, _} ->
        World.background(world)
      {d, obj} -> 
        i = Ray.vector(ray, (d - @delta))
        normal = Sphere.normal(i, obj)
        visible = visible(i, World.lights(world), objects)
        illumination = Light.combine(i, normal,  visible)
        Light.illuminate(obj, illumination, world)
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
    Enum.filter(lights, fn(light) -> clear(point, Light.origin(light), objs) end)
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

end