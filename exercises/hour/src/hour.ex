defmodule Hour do

  @type glass() :: {:glass, integer(), integer()} 

  def solve(k) do
    solve({:glass, 0, 4}, {:glass, 0, 7}, k)
  end
  
  def solve(one, two, k) do
    iterative(one, two, k, 0)
  end

  ## iterative deepening to a depth of 2k - we can not have more than
  ## two flips a minute.
  
  def iterative(_, _, k, z) when z > 2*k do :no end
  def iterative(one, two, k, z) do
    case tock(one, two, k, z) do
      :no ->
	iterative(one, two, k, z+1)
      path ->
	{:solved, z, path}
    end
  end

  ## solve k minutes with exactly z flips

  def tock(one, two, 0, 0) do [{0, one, two, "done"}] end
  def tock(_, _, _, 0) do :no end

  def tock(one, two, k, z)  do  
    case tick(flip(one), two, k, z-1) do
      :no ->
	case tick(one, flip(two), k, z-1) do
	  :no ->
	    if (z >= 2) do
	      case tick(flip(one), flip(two), k, z-2) do
		:no ->
		  :no
		path ->
		  [{k, one, two, "flip both"}|path]
	      end
	    else
	      :no
	    end
	  path ->
	    [{k, one, two, "flip second"}|path]
	end
      path ->
	[{k, one, two, "flip first"}|path]
    end
  end

  
  def tick({:glass, t1, n1}, {:glass, 0, n2}, k, z)  when t1 > 0 and t1 <= k do  
    tock({:glass, 0, n1+t1}, {:glass, 0, n2}, k-t1, z)
  end        
  
  def tick({:glass, 0, n1}, {:glass, t2, n2}, k, z)  when t2 > 0 and t2 <= k do  
    tock({:glass, 0, n1}, {:glass, 0, n2+t2}, k-t2, z)
  end

  def tick({:glass, t1, n1}, {:glass, t2, n2}, k, z) when t1 > 0 and t1 < t2 and t1 <= k do  
    tock({:glass, 0, n1+t1}, {:glass, t2-t1, n2+t1}, k-t1, z)
  end

  def tick({:glass, t1, n1}, {:glass, t2, n2}, k, z) when t2 > 0 and t1 >= t2 and t2 <= k do  
    tock({:glass, t1-t2, n1+t2}, {:glass, 0, n2+t2}, k-t2, z)
  end  

  def tick( _, _, _, _)  do :no end
  

  def flip({:glass, t, n}) do {:glass, n, t} end
  

end
