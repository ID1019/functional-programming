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
  defp split([], l1, l2) do {l1,l2} end
  defp split([x|tail], l1, l2) do split(tail, [x|l2], l1) end

  #Merges two list in order by iterating through them 1 item atime
  defp merge([], l2) do  l2 end
  defp merge(l1, []) do  l1 end
  defp merge([x1|l1], [x2|_]=l2)  when x1 < x2 do [x1|merge(l1, l2)] end
  defp merge(l1, [x2|l2]) do [x2|merge(l1, l2)] end  
  


  #------- Quicksort ---------

  def qsort([]) do [] end

  def qsort([head | tail]) do
    {smaller, greater} = partition(head, tail) #Pick the head as the pivot and seperate the tail into smaller & greater segments
    qsort(smaller) ++ [head] ++ qsort(greater) #Recursively sort both the segments and append the pivot in the middle 
  end

  #We can avoid duplication of code by using a "Anonymous function" that we pass into the "CompareValues" function.
  defp partition(pivot, l) do
    #We pass a function that returns true if "argument 1" is greter then or equal to "argument 2"
    smaller = compareValues(pivot, l, &(&1 >= &2))
    #We pass a function that returns true if "argument 1" is less than "argument 2"
    greater = compareValues(pivot, l, &(&1 < &2))

    #This enables us to not have to implement two seperate functions
    # smaller = compareSmaller(pivot, l)
    # greater = compareGreater(pivot, l)

    {smaller, greater}
  end

  #Here we expect the function "Comp" to be passed as an argument. In this case it will check if the value is lesser or greater then the pivot
  #We then call "Comp" to se if we should add the value to the list or ignore it.
  defp compareValues(pivot, [], comp) do [] end
  defp compareValues(pivot, [head | tail], comp) do
    if(comp.(pivot, head)) do
      [head | compareValues(pivot, tail, comp)]
    else
      compareValues(pivot, tail, comp)
    end
  end


  '''
  #Redundent code that can easily be avoided by using Anonymous functions
  defp compareSmaller(pivot, []) do [] end
  defp compareSmaller(pivot, [head | tail]) do
    if(pivot > head) do
      [head | compareSmaller(pivot, tail)]
    else
      compareSmaller(pivot, tail)
    end
  end

  defp compareGreater(pivot, []) do [] end
  defp compareGreater(pivot, [head | tail]) do
    if(pivot <= head) do
      [head | compareGreater(pivot, tail)]
    else
      compareGreater(pivot, tail)
    end
  end
  '''




  #------- Compressed Quicksort ---------

  #Compressed quick sort using the "Enum.partition", quite neat ey?
  def qsortEnum([]) do [] end
  def qsortEnum([head | tail]) do
    {smaller, greater} = Enum.partition(tail, &(&1 < head))
    qsortEnum(smaller) ++ [head] ++ qsortEnum(greater)  
  end


  
end
