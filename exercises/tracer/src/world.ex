defmodule World do

  @background {0, 0, 0}
  @depth 2
  @ambient {0.3, 0.3, 0.3}

  defstruct objects: [], lights: [], background: @background, depth: @depth, ambient: @ambient

  def world(objects, lights) do
    struct(World, objects: objects, lights: lights)
  end

  def world(objects, lights, opt) do
    depth = case List.keyfind(opt, :depth, 0) do
      {:depth, d} -> d
      nil -> @depth
    end
    background = case List.keyfind(opt, :background, 0) do
      {:background, b} -> b
      nil -> @background
    end
    ambient = case List.keyfind(opt, :ambient, 0) do
      {:ambient, a} -> a
      nil -> @ambient
    end

    struct(World, objects: objects, lights: lights, background: background, depth: depth, ambient: ambient)
  end

  def background(world), do: world.background

  def ambient(world), do: world.ambient

  def lights(world), do: world.lights

  def objects(world), do: world.objects

end