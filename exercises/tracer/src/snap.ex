defmodule Snap do
  
  def snap(0) do
    camera = Camera.normal({800, 600})

    obj1 = %Sphere{radius: 140, pos: {0, 0, 700}}
    obj2 = %Sphere{radius: 50, pos: {200, 0, 600}}
    obj3 = %Sphere{radius: 50, pos: {-80, 0, 400}}

    image = Tracer.tracer(camera, [obj1, obj2, obj3])
    PPM.write("snap0.ppm", image)
  end

  def snap(1) do
    camera = Camera.normal({800, 600})

    obj1 = %Sphere{radius: 140, pos: {0, 0, 700}, color: {1, 0.5, 0}}
    obj2 = %Sphere{radius: 50, pos: {200, 0, 600}, color:  {0, 0.8, 0.2}}
    obj3 = %Sphere{radius: 50, pos: {-80, 0, 400}, color: {0.1, 0.1, 1}}

    image = TracerColor.tracer(camera, [obj1, obj2, obj3])
    PPM.write("snap1.ppm", image)
  end

  def snap(2) do
    camera = Camera.normal({1920, 1080})

    obj1 = %Sphere{radius: 140, pos: {0, 0, 700}, color: {1, 0.5, 0}}
    obj2 = %Sphere{radius: 50, pos: {200, 0, 600}, color: {0, 0.8, 0.2}}
    obj3 = %Sphere{radius: 50, pos: {-80, 0, 400}, color: {0.1, 0.1, 1}}

    light1 = %Light{pos: {-1000, 1000, 700}, color: {1.0, 0.3, 0.3}}
    light2 = %Light{pos: {800, 800, 0}, color: {0.3, 1.0, 0.3}}
    light3 = %Light{pos: {800, -800, 0}, color: {0.3, 0.3, 1.0}}

    world = %World{
      objects: [obj1, obj2, obj3],
      lights: [light1, light2, light3],
      background: {0.0, 0.0, 0.0},
      ambient: {0.6, 0.6, 0.6}}
    image = TracerLight.tracer(camera, world)
    PPM.write("snap2.ppm", image)
  end

  def snap(3) do
    camera = Camera.normal({1920, 1080})

    obj1 = %Sphere{radius: 140, pos: {0, 0, 700}, color: {1, 0.5, 0}, brilliance: 1.0}
    obj2 = %Sphere{radius: 50, pos: {200, 0, 600}, color: {0, 0.8, 0.2}, brilliance: 0.4}
    obj3 = %Sphere{radius: 50, pos: {-80, 0, 400}, color: {0.1, 0.1, 1}, brilliance: 0.8}

    light1 = %Light{pos: {-1000, 1000, 700}, color: {1.0, 0.3, 0.3}}
    light2 = %Light{pos: {800, 800, 0}, color: {0.3, 1.0, 0.3}}
    light3 = %Light{pos: {800, -800, 0}, color: {0.3, 0.3, 1.0}}

    world = %World{
      objects: [obj1, obj2, obj3],
      lights: [light1, light2, light3],
      background: {0.0, 0.0, 0.0},
      ambient: {0.1, 0.1, 0.1}
    }

    image = TracerReflection.tracer(camera, world)
    PPM.write("snap3.ppm", image)
  end

  def snap(4) do
    camera = Camera.normal({1920, 1080})

    obj1 = %Sphere{radius: 140, pos: {  0, 0, 700}, color: {1, 1, 1}, brilliance: 0.4}
    obj2 = %Sphere{radius:  50, pos: {200, 0, 600}, color: {1, 1, 1}, brilliance: 0.8}
    obj3 = %Sphere{radius:  50, pos: {-80, 0, 400}, color: {1, 1, 1}, brilliance: 0.5}

    light1 = %Light{pos: {-1000, 1000, 700}, color: {1.0, 0.3, 0.3}}
    light2 = %Light{pos: {800, 800, 0}, color: {0.3, 1.0, 0.3}}
    light3 = %Light{pos: {800, -800, 0}, color: {0.3, 0.3, 1.0}}

    world = %World{
      objects: [obj1, obj2, obj3],
      lights: [light1, light2, light3],
      background: {0.0, 0.0, 0.0},
      ambient: {0.1, 0.1, 0.1},
      depth: 3
    }

    image = TracerReflection.tracer(camera, world)
    PPM.write("snap4.ppm", image)
  end

  def snap(5) do
    camera = Camera.normal({3200, 1800})

    obj1 = %Sphere{radius: 140, pos: {0, 0, 700}, color: {1.0, 0.5, 0}, brilliance: 0.4}
    obj2 = %Sphere{radius: 50, pos: {200, 0, 600}, color: {0, 0.8, 0.2}, brilliance: 0.4}
    obj3 = %Sphere{radius: 50, pos: {-80, 0, 400}, color: {0.1, 0.1, 1.0}, brilliance: 0.8}

    light1 = %Light{pos: {-1000, 1000, 700}, color: {1.0, 0.3, 0.3}}
    light2 = %Light{pos: {800, 800, 0}, color: {0.3, 1.0, 0.3}}
    light3 = %Light{pos: {800, -800, 0}, color: {0.3, 0.3, 1.0}}

    world = %World{
      objects: [obj1, obj2, obj3],
      lights: [light1, light2, light3],
      background: {0.0, 0.0, 0.0},
      ambient: {0.1, 0.1, 0.1},
      depth: 3
    }

    image = TracerReflection.tracer(camera, world)
    PPM.write("snap5.ppm", image)
  end

  def snap(6) do
    camera = Camera.normal({1920, 1080})

    obj1 = %Sphere{radius: 140, pos: {0, 0, 700}, color: {1.0, 0.5, 0}, brilliance: 1}
    obj2 = %Sphere{radius: 50, pos: {200, 0, 600}, color: {0, 0.8, 0.2}, brilliance: 1}
    obj3 = %Sphere{radius: 50, pos: {-80, 0, 400}, color: {0.1, 0.1, 1.0}, brilliance: 1}

    obj4 = %Plane{pos0: {-20, -20, 500}, pos1: {20,-20,550}, pos2: {0,30,600}, color: {0, 0.2, 0.4}, brilliance: 1}
    
    light1 = %Light{pos: {-1000, 1000, 700}, color: {1.0, 0.3, 0.3}}
    light2 = %Light{pos: {800, 800, 0}, color: {0.3, 1.0, 0.3}}
    light3 = %Light{pos: {800, -800, 0}, color: {0.3, 0.3, 1.0}}

    world = %World{
      objects: [obj1, obj2, obj3, obj4],
      lights: [light1, light2, light3],
      background: {0.0, 0.0, 0.0},
      ambient: {0.1, 0.1, 0.1},
      depth: 3
    }

    image = TracerReflection.tracer(camera, world)
    PPM.write("snap6.ppm", image)
  end

  def snap(7) do
    camera = Camera.normal({1920, 1080})

    obj1 = %Sphere{radius: 140, pos: {-50, 0, 700}, color: {1.0, 0.5, 0}, brilliance: 1}
    obj2 = %Sphere{radius: 50, pos: {200, 0, 600}, color: {0, 0.8, 0.2}, brilliance: 1}
    obj3 = %Sphere{radius: 50, pos: {-80, 0, 400}, color: {0.1, 0.1, 1.0}, brilliance: 1}

    obj4 = %Plane{pos0: {-20, -20, 500}, pos1: {20,-20,550}, pos2: {0,30,600}, color: {0, 0.8,0}, brilliance: 1}

    obj5 = %Plane{pos0: {-50, -200, 900}, pos1: {500,-240,800}, pos2: {200,300,850}, color: {0.1, 0.1, 0.1}, brilliance: 1}    
    
    light1 = %Light{pos: {-1000, 1000, 700}, color: {1.0, 0.3, 0.3}}
    light2 = %Light{pos: {800, 800, 0}, color: {0.3, 1.0, 0.3}}
    light3 = %Light{pos: {800, -800, 0}, color: {0.3, 0.3, 1.0}}

    world = %World{
      objects: [obj1, obj2, obj3, obj4, obj5],
      lights: [light1, light2, light3],
      background: {0.0, 0.0, 0.0},
      ambient: {0.1, 0.1, 0.1},
      depth: 3
    }

    image = TracerReflection.tracer(camera, world)
    PPM.write("snap7.ppm", image)
  end
  



end
