defmodule Hour do

  @type glass() :: {:open, integer(), integer()}


  def solve(k) do
    one = {:open, 7, 0}
    two = {:open, 4, 0}
    case solve(one, two, k) do
      :no ->
	:no
      path ->
	[{k, one, two, "start"} | path]
    end
  end
  
  def solve( _, _, k) when k < 0 do :no end

  def solve( {:open, 0, _}=one,  two, 0) do [{0, one, two, "first empty, done"}] end
  def solve( one,  {:open, 0, _}=two, 0) do [{0, one, two, "second empty, done"}] end  

  def solve( {:open, 0, n1}=one, {:open, 0, n2}=two, k) do  
    case cont({:open, n1, 0}, {:open, n2, 0}, k) do
      :no ->
	:no
      path ->
	[{k, one, two, "both empty, toggle both"}|path]
    end
  end
  
  def solve( {:open, 0, n1}=one, {:open, t2, n2}=two, k) do
    case cont({:open, n1, 0}, {:open, t2, n2}, k) do
      :no ->
	case cont({:open, n1, 0}, {:open, n2, t2}, k) do
	  :no ->
	    :no
	  path ->
	    [{k, one, two, "first empty, toggle both"} | path]
	end
      path ->
	[{k, one, two, "first empty, toggle it"} | path]
    end
  end

  def solve( {:open, t1, n1}=one, {:open, 0, n2}=two, k) do
    case cont({:open, t1, n1}, {:open, n2, 0}, k) do
      :no ->
	case cont({:open, n1, t1}, {:open, n2, 0}, k) do
	  :no ->
	    :no
	  path ->
	    [{k, one, two, "second empty, toggle both"} | path]
	end
      path ->
	[{k, one, two, "second empty, toggle it"} | path]
    end
  end
  
  def solve(one, two, k) do 
    cont(one, two, k)
  end

  def cont( {:open, t1, n1}, {:open, t2, n2}, k) do  
    solve( {:open, t1-1, n1+1}, {:open, t2-1, n2+1}, k-1)
  end    

  

end
