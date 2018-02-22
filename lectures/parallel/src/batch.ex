defmodule Batch do

  def map({:image, header, image}, kernel) do
    {:ok, n, header, kernel} = kernel.(header)
    lines = case n do
	      1 -> map_1(image, kernel)
	      3 -> map_3(image, kernel)
	      5 -> map_5(image, kernel)		
	    end
    {:image, header, lines}
  end

  ## 1x1 kernel 

  def map_1(lines, kernel) do
    Enum.map(lines, fn(line) -> Conv.map(line, kernel) end)
  end

  ## 3x3 kernel

  def map_3([], _) do [] end
  
  def map_3([l1|lines], kernel) do
    ## first line is dublicated
    map_3(l1, l1, lines, kernel)
  end

  def map_3(l1, l2, lines, kernel) do
    case lines do
      [] ->
	[Conv.map(l1,l2,l2,kernel)]
      [l3|lines] ->
	[Conv.map(l1,l2,l3, kernel)| map_3(l2, l3, lines, kernel)]
    end
  end

  ## 5x5 kernel

  def map_5([], _) do [] end
  
  def map_5([l1|lines], kernel) do
    ## first line is triplicated
    map_5(l1, l1, l1, lines, kernel)
  end

  
  def map_5(l1, l2, l3, lines, kernel) do
    case lines do
      [] ->
	[Conv.map(l1,l2,l3,l3,l3,kernel)]	
      [l4|lines] ->
	map_5(l1, l2, l3, l4, lines, kernel) 
    end
  end    

  def map_5(l1, l2, l3, l4, lines, kernel) do
    case lines do
      [] ->
	[Conv.map(l1,l2,l3,l4,l4,kernel),  Conv.map(l2, l3, l4, l4, l4, kernel)]
      [l5|lines] ->
	[Conv.map(l1,l2,l3,l4,l5,kernel)| map_5(l2, l3, l4, l5, lines, kernel)]
    end
  end

end

