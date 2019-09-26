defmodule Bench do

  def test() do
    {time, _} = :timer.tc(fn() -> image() end)
    :io.format("execution time: ~.2f\n", [time/1000000])
    time
  end

  def image() do
    camera = Camera.normal({1920, 1080})

    obj1 = %Sphere{radius: 140, pos: {0, 0, 700}, color: {1, 0.5, 0}}
    obj2 = %Sphere{radius: 50, pos: {200, 0, 600}, color:  {0, 0.8, 0.2}}
    obj3 = %Sphere{radius: 50, pos: {-80, 0, 400}, color: {0.1, 0.1, 1}}

    TracerColor.tracer(camera, [obj1, obj2, obj3])
  end
  

end


 
