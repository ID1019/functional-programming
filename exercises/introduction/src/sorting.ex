defmodule Sorting do

  #------- Insertion Sort ---------

  def isort(l) do isort(l, []) end
  
  defp isort([], sorted) do sorted end
  defp isort([head | tail], sorted) do isort(tail, insert(head, sorted)) end

  #Returns a lise where x has been inserted into the first place where it is smaller then the next integer.
  defp insert(x, []) do [x] end
  defp insert(x, [head | tail]) when x < head do [x, head | tail] end
  defp insert(x, [head | tail]) do [head | insert(x, tail)] end
  

  #------- Merge Sort ---------


  #Returns a sorted list using the merge sort algorithm.
  def msort([]) do [] end
  def msort([x]) do [x] end
  def msort(l) do
    {l1, l2} = split(l, [], [])
    merge(msort(l1), msort(l2))
  end
  
  #Split a list into two equal sized lists using Enum
  def split([], l1, l2) do {l1,l2} end
  def split([x|tail], l1, l2) do split(tail, [x|l2], l1) end

  #Merges two list in order by iterating through them 1 item atime
  def merge([], l2) do  l2 end
  def merge(l1, []) do  l1 end
  def merge([x1|l1], [x2|_]=l2)  when x1 < x2 do [x1|merge(l1, l2)] end
  def merge(l1, [x2|l2]) do [x2|merge(l1, l2)] end  
  
  
end
