defmodule Acc do

  def sum([]) do 0 end
  def sum([n|t]) do 
    n + sum(t) 
  end

  def odd([]) do  [] end
  def odd([h|t]) do
    if rem(h,2) == 1 do
      [h|odd(t)]
    else 
      odd(t)
   end
  end

  def even([]) do  [] end
  def even([h|t]) do
    if rem(h,2) == 0 do
      [h|even(t)]
    else 
      even(t)
   end
  end  

  def split(l) do
    odd = odd(l)
    even = even(l)
    {odd, even}
  end


  def odd_n_even([]) do  {[], []} end
  def odd_n_even([h|t]) do
    {odd, even} = odd_n_even(t)
    if rem(h,2) == 1 do
      {[h|odd], even}
    else 
      {odd, [h|even]}      
    end
  end

end
