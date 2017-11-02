-module(snap).

-compile(export_all).

snap(0) ->
    Camera = camera:normal({800,600}),

    Obj1 = objects:sphere( 140, {    0,    0, 700}),
    Obj2 = objects:sphere(  50, {  200,    0, 600}),
    Obj3 = objects:sphere(  50, {  -80,    0, 400}),

    %% We us the simple trace.erl that returns a black and white image

    Image = tracer:tracer(Camera, [Obj1, Obj2, Obj3]),
    ppm:write("test0.ppm", Image);

snap(1) ->
    Camera = camera:normal({800,600}),

    Obj1 = objects1:sphere( 140, {   0,    0, 700}, [{color, {1,  0.5,   0}}]),
    Obj2 = objects1:sphere(  50, { 200,    0, 600}, [{color, {0,  0.8, 0.2}}]),
    Obj3 = objects1:sphere(  50, { -80,    0, 400}, [{color, {0.1,0.1, 1.0}}]),

    %% trace1.erl will use the color of the objects.

    Image = tracer1:tracer(Camera, [Obj1, Obj2, Obj3]),
    ppm:write("snap1.ppm", Image);

snap(2) ->
    Camera = camera:normal({1920,1080}),

    Obj1 = objects1:sphere( 140, {   0,    0,  700}, [{color, {1.0,0.5,0}}]),
    Obj2 = objects1:sphere(  50, { 200,    0,  600}, [{color, {0,0.8,0.2}}]),
    Obj3 = objects1:sphere(  50, { -80,    0,  400}, [{color, {0.1,0.1,1.0}}]),

    Light1 = lights1:light({-1000,  1000,   700}, {1.0,0.3,0.3}),
    Light2 = lights1:light({  800,   800,   0}, {0.3,1.0,0.3}),
    Light3 = lights1:light({  800,  -800,   0}, {0.3,0.3,1.0}),

    % The world will hold all objects, lights etc.

    World = world1:world([Obj1, Obj2, Obj3], [Light1, Light2, Light3], [{background, {0.0,0.0,0.0}}, {ambient, {0.6,0.6,0.6}}]),

    %% trace2.erl will use colored lights in combination with the color of the object

    Image = tracer3:tracer(Camera, World),
    ppm:write("snap2.ppm", Image);

snap(3) ->
    Camera = camera:normal({1920,1080}),

    Obj1 = objects1:sphere( 140, {   0, 0, 700}, [{color, {0.8,0.3,0.3}}, {brilliance, 1.0}]),
    Obj2 = objects1:sphere(  50, { 200, 0, 600}, [{color, {0.0,0.8,0.2}}, {brilliance, 0.4}]),
    Obj3 = objects1:sphere(  50, { -80, 0, 400}, [{color, {0.1,0.1,1.0}}, {brilliance, 0.8}]),


    Light1 = lights1:light({-1000, -1000,  300}, {1.0,0.0,0.0}),
    Light2 = lights1:light({  800, 800,   0}, {0.3,1.0,0.3}),
    Light3 = lights1:light({  800,-800,   0}, {0.3,0.3,1.0}),

    World = world1:world([Obj1, Obj2, Obj3], [Light1, Light2, Light3], [{depth, 2}, {background, {0.0,0.0,0.1}}, {ambient, {0.1,0.1,0.1}}]),

    %% trace3.erl will use reflection 

    Image = tracer3:tracer(Camera, World),
    ppm:write("snap3.ppm", Image);

snap(4) ->
    Camera = camera:normal({1920,1080}),

    Obj1 = objects1:sphere( 140, {   0, 0, 700}, [{color, {1  , 1,  1}}, {brilliance, 0.4}]),
    Obj2 = objects1:sphere(  50, { 200, 0, 600}, [{color, {1  , 1,  1}}, {brilliance, 0.8}]),
    Obj3 = objects1:sphere(  50, { -80, 0, 400}, [{color, {1  , 1,  1}},  {brilliance, 0.5}]),

    Light1 = lights1:light({-1000, -1000,  700}, {1.0,0.0,0.0}),
    Light2 = lights1:light({  800,   800,  0},   {0.1,1.0,0.0}),
    Light3 = lights1:light({  800,  -800,  0},   {0.0,0.0,1.0}),

    %% We increase the reflection to 3.

    World = world1:world([Obj1, Obj2, Obj3], [Light1, Light2, Light3], [{depth, 3}, {background, {0.0,0.0,0.0}}, {ambient, {0.1,0.1,0.1}}]),

    Image = tracer3:tracer(Camera, World),
    ppm:write("snap4.ppm", Image);

snap(5) ->
    %% We increase the size of the canvas.
    Camera = camera:normal({3200,1800}),

    Obj1 = objects1:sphere( 140, {   0, 0, 700}, [{color, {1.0,0.5,0}}, {brilliance, 0.4}]),
    Obj2 = objects1:sphere(  50, { 200, 0, 600}, [{color, {0,0.8,0.2}}, {brilliance, 0.4}]),
    Obj3 = objects1:sphere(  50, { -80, 0, 400}, [{color, {0.1,0.1,1.0}}, {brilliance, 0.8}]),

    Light1 = lights1:light({-1000,-1000, 700}, {1.0,0.3,0.3}),
    Light2 = lights1:light({ 800, 800,  0}, {0.3,1.0,0.3}),
    Light3 = lights1:light({ 800,-800,  0}, {0.3,0.3,1.0}),

    World = world1:world([Obj1, Obj2, Obj3], [Light1, Light2, Light3], [{depth, 3}, {background, {0.0,0.0,0.0}}, {ambient, {0.1,0.1,0.1}}]),

    Image = tracer3:tracer(Camera, World),
    ppm:write("snap5.ppm", Image).








    





