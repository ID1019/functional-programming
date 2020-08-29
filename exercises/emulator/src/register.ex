defmodule Register do

  def new() do
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
  end
  
  def read(  _, 0) do 0 end  
  def read(reg, i) do elem(reg, i) end

  def write(reg, 0, _) do reg end
  def write(reg, i, val) do put_elem(reg, i, val) end  


end
