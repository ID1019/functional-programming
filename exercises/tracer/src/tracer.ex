defmodule Tracer do

  @black {0, 0, 0}
  @white {1, 1, 1}

  def tracer(camera=%Camera{}, objects) do
    {w, h} = camera.size
    xs = Enum.to_list(1..w)
    ys = Enum.to_list(1..h)
    for y <- ys, do: for(x <- xs, do: trace(x, y, camera, objects))
  end

  def trace(x, y, camera, objects) do
    ray = Camera.ray(camera, x, y)
    trace(ray, objects)
  end

  def trace(ray, objects) do
    case intersect(ray, objects) do
      {:inf, _} ->
        @black

      {_, _} ->
        @white
    end
  end

  def intersect(ray, objects) do
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

end
