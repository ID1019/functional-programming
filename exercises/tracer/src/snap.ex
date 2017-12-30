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

  def snap(2) do
    camera = Camera.normal({1920, 1080})

    obj1 = Sphere.sphere(140, {0, 0, 700}, [{:color, {1, 0.5, 0}}])
    obj2 = Sphere.sphere(50, {200, 0, 600}, [{:color, {0, 0.8, 0.2}}])
    obj3 = Sphere.sphere(50, {-80, 0, 400}, [{:color, {0.1, 0.1, 1}}])

    light1 = Light.light({-1000, 1000, 700}, {1.0, 0.3, 0.3})
    light2 = Light.light({800, 800, 0}, {0.3, 1.0, 0.3})
    light3 = Light.light({800, -800, 0}, {0.3, 0.3, 1.0})

    world = World.world([obj1, obj2, obj3], [light1, light2, light3], [{:background, {0.0, 0.0, 0.0}}, {:ambient, {0.6, 0.6, 0.6}}])

    image = TracerLight.tracer(camera, world)
    PPM.write("snap2.ppm", image)
  end

end