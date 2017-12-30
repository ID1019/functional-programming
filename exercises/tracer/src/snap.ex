defmodule Snap do

  def snap(0) do
    camera = Camera.normal({800, 600})

    obj1 = Sphere.sphere(140, {0, 0, 700})
    obj2 = Sphere.sphere(50, {200, 0, 600})
    obj3 = Sphere.sphere(50, {-80, 0, 400})

    image = Tracer.tracer(camera, [obj1, obj2, obj3])
    PPM.write("snap0.ppm", image)
  end

  def snap(1) do
    camera = Camera.normal({800, 600})

    obj1 = Sphere.sphere(140, {0, 0, 700}, [{:color, {1, 0.5, 0}}])
    obj2 = Sphere.sphere(50, {200, 0, 600}, [{:color, {0, 0.8, 0.2}}])
    obj3 = Sphere.sphere(50, {-80, 0, 400}, [{:color, {0.1, 0.1, 1}}])

    image = TracerColor.tracer(camera, [obj1, obj2, obj3])
    PPM.write("snap1.ppm", image)
  end
end