defmodule Filter do

  ## 1x1 kernels

  def rgb_to_gray() do
    fn({:rgb, size, depth}) ->
      {:ok, 1,
       {:gray, size, depth}, 
       fn({r,g,b}) ->
	 div(r+g+b, 3)
       end}
    end
  end

  def gray_reduce() do
    fn({:gray, size, 255}) ->
      {:ok, 1,
       {:gray, size, 3}, 
       fn(d) ->
	 div(d, 64)
       end}
    end
  end

  def gray_invert() do
    fn({:gray, size, depth}) ->
      {:ok, 1,
       {:gray, size, depth}, 
       fn(d) ->
           depth - d
       end}
    end
  end

  
  ## 3x3 kernels 

  def gray_edge() do
    fn ({:gray, size, depth}) ->
      {:ok, 3,
       {:gray, size, depth},
       fn(lines) -> Kern.fold([ 0, 1, 0,
  			        1,-4, 1,
			        0, 1, 0], lines, 0, depth) end}
    end
  end


  def rgb_edge() do
    fn ({:rgb, size, depth}) ->
      {:ok, 3,
       {:rgb, size, depth},
       fn(lines) -> Kern.fold([ 0, 1, 0,
  			        1,-4, 1,
			        0, 1, 0], lines, {0,0,0}, depth) end}
    end
  end



  
  def rgb_sharp() do
    fn ({:rgb, size, depth}) ->
      {:ok, 3,
       {:rgb, size, depth},
       fn(lines) -> Kern.fold([  0,-1, 0,
				-1, 5,-1,
			         0,-1, 0], lines, {0,0,0}, depth) end}
    end
  end  

  ## 5x5 kernels
  
  def rgb_blur() do
    fn ({:rgb, size, depth}) ->
      {:ok, 5,
       {:rgb, size, depth},       
       fn(lines) -> Kern.fold([ 0.04, 0.04, 0.04, 0.04, 0.04,
			        0.04, 0.04, 0.04, 0.04, 0.04,
			        0.04, 0.04, 0.04, 0.04, 0.04,
			        0.04, 0.04, 0.04, 0.04, 0.04,
			        0.04, 0.04, 0.04, 0.04, 0.04], lines, {0,0,0}, depth) end}
    end
  end

  def rgb_motion() do
    fn ({:rgb, size, depth}) ->
      {:ok, 5,
       {:rgb, size, depth},       
       fn(lines) -> Kern.fold([ 0.2, 0.0, 0.0, 0.0, 0.0				,
			        0.0, 0.2, 0.0, 0.0, 0.0,
			        0.0, 0.0, 0.2, 0.0, 0.0,
			        0.0, 0.0, 0.0, 0.2, 0.0,
			        0.0, 0.0, 0.0, 0.0, 0.2], lines, {0,0,0}, depth) end}
    end
  end
    
  
end
