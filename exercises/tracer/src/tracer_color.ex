defmodule TracerColor do
  
  @black {0, 0, 0}

  
  def tracer(camera, objects) do
    {w, h} = camera.size
    for y <- 1..h, do: for(x <- 1..w, do: trace(x, y, camera, objects))
  end

  def trace(x, y, camera, objects) do
    ray = Camera.ray(camera, x, y)
    trace(ray, objects)
  end
  def trace(ray, objects) do
    case intersect(ray, objects) do
      {:inf, _} ->
        @black

      {_, obj} ->
         obj.color	
    end
  end

  def intersect(ray, objects) do
    List.foldl(objects, {:inf, :no}, fn object, sofar ->
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
