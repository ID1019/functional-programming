defmodule Strm do

  def start(kernel, out) do
    spawn_link(fn() -> init(kernel, out) end)
  end

  def init(kernel, out) do
    receive do
      {:header, header} ->
	{:ok, n, header, kernel} = kernel.(header)
	send(out, {:header, header})
	case n do
	  1 -> map_1(kernel, out)
	  3 -> map_3(kernel, out)
	  5 -> map_5(kernel, out)	  	  
	end
    end
  end

  ## 1x1 kernel 
  
  def map_1(kernel, out) do
    receive do
      :done ->
	send(out, :done)
      {:line, line} ->
	send(out, {:line, Conv.map(line, kernel)})
	map_1(kernel, out)
    end
  end

  ## 3x3 kernel

  def map_3(kernel, out) do
    receive do
      :done ->
	send(out, :done)
      {:line, l1} ->
	## first line is dublicated
	map_3(l1, l1, kernel, out)
    end
  end

  def map_3(l1, l2, kernel, out) do
    receive do
      :done ->
	## last line is duplicated
	send(out, {:line, Conv.map(l1,l2,l2, kernel)})
	send(out, :done)
      {:line, l3} ->
	send(out, {:line, Conv.map(l1,l2,l3, kernel)})
	map_3(l2, l3, kernel, out)
    end
  end


  ## 5x5 kernel

  def map_5(kernel, out) do
    receive do
      :done ->
	send(out, :done)
      {:line, l1} ->
	## first line is triplicated
	map_5(l1, l1, l1, kernel, out)
    end
  end

  def map_5(l1, l2, l3, kernel, out) do
    receive do
      :done ->
	## last line is triplicated
	send(out, {:line, Conv.map(l1,l2,l3,l3,l3,kernel)})
	send(out, :done)
      {:line, l4} ->
	map_5(l1, l2, l3, l4, kernel, out)
    end
  end

  def map_5(l1, l2, l3, l4, kernel, out) do
    receive do
      :done ->
	## last line is triplicated
	send(out, {:line, Conv.map(l1,l2,l3,l4,l4,kernel)})
	send(out, {:line, Conv.map(l2,l3,l4,l4,l4,kernel)})
	send(out, :done)
      {:line, l5} ->
	send(out, {:line, Conv.map(l1,l2,l3,l4,l5,kernel)})
	map_5(l2, l3, l4, l5, kernel, out)
    end
  end  
  

  
end
