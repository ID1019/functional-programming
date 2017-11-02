-module(test).

-compile(export_all).

snap(0) ->
    Camera = camera:normal({800,600}),

    Obj1 = objects:sphere( 140, {    0,    0, 700}),
    Obj2 = objects:sphere(  50, {  200,    0, 600}),
    Obj3 = objects:sphere(  50, {  -80,    0, 400}),

    %% We us the simple trace.erl that returns a black and white image

    Image = tracer:tracer(Camera, [Obj1, Obj2, Obj3]),
    ppm:write("test0.ppm", Image).









    





