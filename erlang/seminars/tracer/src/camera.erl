-module(camera).

-export([camera/5, normal/1, size/1, ray/3]).

-record(camera, {pos,    %  the position of the camera
		 corner, % the upper left corner of the plane
		 right,  % the vector in the plane from the corner going right
		 down,   % the vector in the  plane from the corner going down
		 size    % the {width, height}
		}).

camera(Pos, Corner, Right, Down, Size) ->
    #camera{pos = Pos, 
	    corner = Corner,
	    right = Right, 
	    down = Down, 
	    size = Size}.

normal(Size) ->
    {Width, Height} = Size,
    D = Width * 1.2,
    H = Width / 2,
    V = Height /  2,
    Corner = {-H,V,D},
    camera({0,0,0}, Corner, {1,0,0}, {0,-1,0}, Size).


size(#camera{size=Size}) ->
    Size.

ray(X, Y, Camera) ->
    Origin = Camera#camera.pos,
    Xpos = vector:smul(Camera#camera.right, X),
    Ypos = vector:smul(Camera#camera.down, Y),
    Pixle = vector:add(Camera#camera.corner, vector:add(Xpos, Ypos)),
    Dir = vector:normalize(Pixle),
    objects:ray(Origin, Dir).

