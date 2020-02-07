defmodule Plane do

  @color {1.0, 0.4, 0.4}
  @brilliance 0
  @transparency 0
  @refraction 1.5

  
  defstruct(
    pos0: {0, 0, 0},
    pos1: {1, 0, 0},
    pos2: {0, 1, 0},
    color: @color,
    brilliance: @brilliance,
    transparency: @transparency,
    refraction: @refraction
  )
    

  defimpl Object do

    def intersect(plane, ray) do
      Plane.intersect(plane, ray)
    end

    def normal(plane, ray, _pos) do
      #  this is not correct, we need to know from which way we're looking
      dir01 = Vector.sub(plane.pos1, plane.pos0)
      dir02 = Vector.sub(plane.pos2, plane.pos0)      
      norm1 = Vector.normalize(Vector.cross(dir01, dir02))
      norm2 = Vector.normalize(Vector.cross(dir02, dir01))
      if Vector.norm(Vector.add(ray.dir,norm1)) < Vector.norm(Vector.add(ray.dir,norm2)) do
	norm1
      else
	norm2
      end
    end

  end

  def intersect(plane, ray) do

    p0 = plane.pos0
    p1 = plane.pos1
    p2 = plane.pos2    

    p01 = Vector.sub(p1,p0)
    p02 = Vector.sub(p2,p0)

    pos = ray.pos

    dir = ray.dir

    neg = Vector.smul(dir, -1)
    
    det = Vector.dot(neg, Vector.cross(p01, p02))

    org = Vector.sub(pos,p0)
    
    if det != 0 do
      t = Vector.dot(Vector.cross(p01, p02), org) / det
      #IO.write("t =  #{t} \n")
      if t > 0 do
	u = Vector.dot(Vector.cross(p02, neg), org) / det
	v = Vector.dot(Vector.cross(neg, p01), org) / det

	#IO.write("u =  #{u} \n")
	#IO.write("v =  #{v} \n")	
	if 0 < u and u < 1 and 0 < v and v < 1  and (u + v) <= 1  do
	  # we have an intersection
	  {:ok, t }
	else
	  :no
	end
      else
	# behind the camera
	:no
      end
    else
      # parallel or in plane
      :no
    end
  end


  

  
end
