defmodule Huffman do

  def cost(seq) do
    huff(Enum.map( Enum.sort(seq), fn x -> {:huf, x, 0, x} end))
  end

  def huff([huf]) do  huf end

  def huff([{:huf, l1, c1, s1}, {:huf, l2, c2, s2} | seq]) do
    huff(insert({:huf, l1+l2, c1+c2+l1+l2, {s1,s2}}, seq))
  end


  def insert(huf, []) do [huf] end
  def insert({:huf, l1, c1, s1}, [{:huf, l2, c2, s2} | seq]) when l1 > l2 do
    [{:huf, l2, c2, s2} | insert({:huf,l1,c1,s1}, seq)]
  end  
  def insert({:huf,l1,c1,s1}, seq) do
    [{:huf, l1, c1, s1} | seq]
  end        

end

