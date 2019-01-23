defmodule Sorting do
  
  # ================================================== #
  # == Insertion Sort == #
  # ================================================== #

  def isort(l) do isort(l, []) end

  def isort([], sorted) do sorted end
  def isort([head | tail], sorted) do
    isort(tail, insert(head, sorted))
  end

  # Returns a list where x has been inserted into the first
  # place where it is smaller then the next integer.
  def insert(x, []) do [x] end
  def insert(x, [head | tail]) when x > head do
    [head | insert(x, tail)]   
  end
  def insert(x, larger) do
    [x | larger]
  end

  # ================================================== #
  # == Merge Sort == #
  # ================================================== #

  # Returns a sorted list using the merge sort algorithm.
  def msort([]) do [] end
  def msort([x]) do [x] end
  def msort(l) do
    {l1, l2} = split(l, [], [])
    merge(msort(l1), msort(l2))
  end

  # Split a list into two equal sized lists using Enum.
  def split([], l1, l2) do {l1, l2} end
  def split([x | tail], l1, l2) do
    split(tail, [x | l2], l1)
  end

  # Merges two list in order by iterating through them one
  # item at time.
  def merge([], l2) do l2 end
  def merge(l1, []) do l1 end
  def merge([x1 | l1], [x2 | _] = l2) when x1 < x2 do
    [x1 | merge(l1, l2)]
  end
  def merge(l1, [x2 | l2]) do
    [x2 | merge(l1, l2)]
  end

  # ================================================== #
  # == Quicksort == #
  # ================================================== #

  # Quicksort (BASIC)
  # Option 1: simple control structures

  def qsort([]) do [] end
  def qsort([p | l]) do 
    {list1, list2} = qsplit(p, l, [], [])
    small = qsort(list1)
    large = qsort(list2)
    append(small, [p | large])
  end

  def qsplit(_, [], small, large) do {small, large} end
  def qsplit(p, [h | t], small, large) do
    if h < p  do
      qsplit(p, t, [h | small], large)
    else
      qsplit(p, t, small, [h | large])
    end
  end

  def append(list1, list2) do
    case list1 do
      [] -> list2
      [h | t] -> [h | append(t, list2)]
    end
  end

  # Quicksort (INTERMEDIATE)
  # Option 2: function capture

  def qsort_capture([]) do [] end
  def qsort_capture([head | tail]) do
    # Pick the head as the pivot and seperate the tail into
    # smaller and greater segments.
    {smaller, greater} = partition(head, tail)
    # Recursively sort both the segments and append the
    # pivot in the middle.
    qsort_capture(smaller) ++ [head] ++ qsort_capture(greater)
  end

  # We can avoid duplication of code by using a "anonymous
  # function" that we pass into the "CompareValues" function.
  def partition(pivot, l) do
    # We pass a function that returns true if "argument 1"
    # is greter then or equal to "argument 2".
    smaller = compareValues(pivot, l, &(&1 >= &2))
    # We pass a function that returns true if "argument 1"
    # is less than "argument 2".
    greater = compareValues(pivot, l, &(&1 < &2))

    # This enables us to not have to implement two seperate 
    # functions:
    # smaller = compareSmaller(pivot, l)
    # greater = compareGreater(pivot, l)

    {smaller, greater}
  end

  # Here we expect the function "Comp" to be passed as an
  # argument. In this case it will check if the value is
  # lesser or greater then the pivot. We then call "Comp"
  # to se if we should add the value to the list or ignore it.
  def compareValues(_pivot, [], _comp) do [] end
  def compareValues(pivot, [head | tail], comp) do
    if comp.(pivot, head) do
      [head | compareValues(pivot, tail, comp)]
    else
      compareValues(pivot, tail, comp)
    end
  end

  # Quicksort (ADVANCED)
  # Option 3: Enum.partition and function capture

  # Compressed quick sort using the "Enum.partition", quite neat ey?
  def qsort_enum([]) do [] end
  def qsort_enum([head | tail]) do
    {smaller, greater} = Enum.split_with(tail, &(&1 < head))
    qsort_enum(smaller) ++ [head] ++ qsort_enum(greater)
  end
end
