defmodule Plane do

  @color {1.0, 0.4, 0.4}
  @brilliance 0
  @transparency 0
  @refraction 1.5

  require Ray
  
  defstruct(pos: {0, 0, 0},
    dir1: {1,0,0},
    dir2: {0,1,0},
    ext1: 200,
    ext2: 100,
    normal: {0,0,1},
    color: @color,
    brilliance: @brilliance,
    transparency: @transparency,
    refraction: @refraction)
    

  defimpl Object do

    def intersect(plane, ray) do
      Plane.intersect(plane, ray)
    end

    def color(plane) do
      plane.color
    end

    def brilliance(plane) do
      plane.brilliance
    end

    def transparency(plane) do
      plane.transparency
    end
    
    def normal(plane, _pos) do
      plane.normal
    end

  end

  def intersect(_plane, _ray) do
    ## When does a ray intersect a plane - and in our case, is it
    ## within the boundries of the extent of the surface. 
    :no
  end

  def surface(_pos, _with, _height) do
    ## Define a plane that has one corner in pos and extend in the
    ## direction width and height. You will have to calculate the two
    ## unit vectors dir1 and dir2, the extent of the surface and the
    ## normal vector.
    
  end
    


  

  
end
