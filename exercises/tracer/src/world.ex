defmodule World do

  @Background {0, 0, 0}
  @Depth 2
  @Ambient {0.3, 0.3, 0.3}

  defstruct objects: [], lights: [], background: @Background, depth: @Depth, ambient: @Ambient

  def world(objects, lights) do
    struct(User, objects: objects, lights: lights)
  end

  def world(objects, lights, opt) do
    depth = case List.keyfind(opt, :depth, 0) do
      {:depth, d} -> d
      :false -> @Depth
    end
    background = case List.keyfind(opt, :background, 0) do
      {:background, b} -> b
      :false -> @Background
    end
    ambient = case List.keyfind(opt, :ambient, 0) do
      {:ambient, a} -> a
      :false -> @Ambient
    end

    struct(User, objects: objects, lights: lights, background: b, depth: d, ambient: a)
  end

  def background() do
    
  end
end