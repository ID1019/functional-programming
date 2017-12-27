defmodule Sorting do

  #------- Insertion Sort ---------

  #Returns a lise where x has been inserted into the first place where it is smaller then the next integer.
  defp insert(x, []), do: [x]
  defp insert(x, [head | tail]) when x < head, do: [x] ++ [head] ++ tail
  defp insert(x, [head | tail]), do: [head | insert(x, tail)]

  def iSort([], sortedList), do: sortedList
  def iSort([head | tail], sortedList), do: iSort(tail, insert(head, sortedList))
  def iSort([]), do: []
  def iSort(l), do: iSort(l, [])




  #------- Merge Sort ---------

  #Split a list in two equal sized lists using Enum
  defp splitL(l) do
    half = round((length(l) / 2))
    Enum.chunk(l, half, half, [])
  end

  #Merges two list in order by iterating through them 1 item atime
  defp listMerge(l1, l2), do: listMerge(l1, l2, [])
  #If one list is empty we are done, and can therefore simply append all lists
  defp listMerge(l1, l2, mergedList) when length(l1) == 0 or length(l2) == 0, do: mergedList ++ l1 ++ l2

  #Compare the first value in each list, we append the smaller to mergedList.
  defp listMerge([h1 | t1], [h2 | t2], mergedList) do
    if(h1 < h2) do
      listMerge(t1, [h2 | t2], mergedList ++ [h1])
    else
      listMerge([h1 | t1], t2, mergedList ++ [h2])
    end
  end

  #Returns a sorted list using the merge sort algorithm.
  def mSort(l) when length(l) <= 1, do: l
  def mSort(l) do
    [head, tail] = splitL(l)
    listMerge(mSort(head), mSort(tail))
  end

  
end