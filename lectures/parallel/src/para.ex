defmodule Para do

  def map({:image, header, image}, kernel) do
    {:ok, n, header, kernel} = kernel.(header)
    lines = case n do
	      1 -> par_1(image, kernel)
	      3 -> par_3(image, kernel)
	      5 -> par_5(image, kernel)		
	    end
    {:image, header, lines}
  end

  ## 1x1 kernel   

  def par_1(lines, kernel) do
    refs = Enum.map(lines, fn(line) -> Async.eval(fn() -> Conv.map(line, kernel) end) end)
    Enum.map(refs, fn(ref) -> Async.collect(ref) end)
  end
  
  
  ## 3x3 kernel


  def par_3(lines, kernel) do
    refs = map_3(lines, kernel)
    Enum.map(refs, fn(ref) -> Async.collect(ref) end)    
  end
  
  
  def map_3([], _) do [] end
  
  def map_3([l1|lines], kernel) do
    ## first line is dublicated
    map_3(l1, l1, lines, kernel)
  end

  def map_3(l1, l2, lines, kernel) do
    case lines do
      [] ->
	[Async.eval(fn() -> Conv.map(l1,l2,l2,kernel) end)]
      [l3|lines] ->
	[Async.eval(fn() -> Conv.map(l1,l2,l3, kernel) end)| map_3(l2, l3, lines, kernel)]
    end
  end

  ## 5x5 kernel

  def par_5(lines, kernel) do
    refs = map_5(lines, kernel)
    Enum.map(refs, fn(ref) -> Async.collect(ref) end)    
  end
	 
  def map_5([], _) do [] end
  
  def map_5([l1|lines], kernel) do
    ## first line is triplicated
    map_5(l1, l1, l1, lines, kernel)
  end

  
  def map_5(l1, l2, l3, lines, kernel) do
    case lines do
      [] ->
	[Async.eval(fn() -> Conv.map(l1,l2,l3,l3,l3,kernel) end)]	
      [l4|lines] ->
	map_5(l1, l2, l3, l4, lines, kernel) 
    end
  end    

  def map_5(l1, l2, l3, l4, lines, kernel) do
    case lines do
      [] ->
	[Async.eval(fn() -> Conv.map(l1,l2,l3,l4,l4,kernel) end),  Async.eval(fn() ->Conv.map(l2, l3, l4, l4, l4, kernel) end)]
      [l5|lines] ->
	[Async.eval(fn() -> Conv.map(l1,l2,l3,l4,l5,kernel) end) | map_5(l2, l3, l4, l5, lines, kernel)]
    end
  end

end

