defmodule Test do

  def snap do
    camera = Camera.normal({800, 600})

    obj1 = Sphere.sphere(140, {0, 0, 700})
    obj2 = Sphere.sphere(50, {200, 0, 600})
    obj3 = Sphere.sphere(50, {-80, 0, 400})

    image = Tracer.tracer(camera, [obj1, obj2, obj3])
    PPM.write("test.ppm", image)
  end

end