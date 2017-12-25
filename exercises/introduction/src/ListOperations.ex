defmodule ListOperations do

  #Returns the n´th elemt of the list.
  def nth(0, [head | tail]), do: head  
  def nth(n, [head | tail]), do: nth(n-1, tail)


  #Returns the number of elements in the list.
  def len([]), do: 0
  def len([head | tail]), do: 1 + len(tail)


  #Returns the sum of all the lements in the list.
  def sum([]), do: 0
  def sum([head | tail]), do: head + sum(tail)


  #Returns a list where all the elements are duplicated.
  def duplicate([]), do: []
  def duplicate([head | tail]), do: [head, head] ++ duplicate(tail)


  #Adds the element x to the list, if it does not already exist in the list.
  def add(x, []), do: [x]
  def add(x, [head | tail]) when x === head, do: [head] ++ tail
  def add(x, [head | tail]), do: [head] ++ add(x, tail)


  #Returns a list of uniqe elements in the list l.
  def unique([], newList), do: newList
  def unique([head | tail]), do: unique(tail, add(head, []))
  def unique([head | tail], newList), do: unique(tail, add(head, newList))


  #Removes all instances of the element x from the list.
  def remove([], x), do: []
  def remove([head | tail], x) when head == x, do: remove(tail, x)
  def remove([head | tail], x), do: [head] ++ remove(tail, x)

  #Returns a list of all instances matching x
  defp match([], x), do: []
  defp match([head | tail], x) when x === head, do: [x] ++ match(tail, x)
  defp match([head | tail], x), do: match(tail, x)

  #Returns a list containing lists of equal elements.
  def pack([]), do: []
  def pack([head | tail]), do: [[head] ++ match(tail, head)] ++ pack(remove(tail, head))

 
  #Returns a list where the order of the elements are reversed.
  def reverse([]), do: []
  def reverse([head | tail]), do: reverse(tail) ++ [head]

end
