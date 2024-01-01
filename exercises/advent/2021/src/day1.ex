defmodule Day1 do

    def input() do
    File.read!("day1.csv")
  end

  def sample() do
    "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"
  end

  def task_a() do
    input = input()
    elfs = String.split(input, "\n\n")
    elfs = sum_each(elfs)
    most(elfs)
  end

  def task_b() do
    input = input()
    elfs = String.split(input, "\n\n")
    elfs = sum_each(elfs)
    {a, b, c}  = top_three(elfs)
    a + b + c
  end

  def most([]) do 0 end
  def most([a|rest]) do
    max(a, most(rest))
  end
  

  def top_three([a|rest]) do top_three(rest, a) end

  def top_three([b|rest], a) when b < a do top_three(rest, b, a) end
  def top_three([b|rest], a) do top_three(rest, a, b) end    

  def top_three([c|rest], a, b) when c < a do top_three(rest, c, a, b) end
  def top_three([c|rest], a, b) when c < b do top_three(rest, a, c, b) end  
  def top_three([c|rest], a, b) do top_three(rest, a, b, c) end

  def top_three([], a, b, c) do {a, b, c} end  
  def top_three([d|rest], a, b, c) when d < a do top_three(rest, a, b, c) end
  def top_three([d|rest], _, b, c) when d < b do top_three(rest, d, b, c) end  
  def top_three([d|rest], _, b, c) when d < c do top_three(rest, b, d, c) end
  def top_three([d|rest], _, b, c) do top_three(rest, b, c, d) end  
  
  
  def sum_each([]) do [] end
  def sum_each([elf|rest]) do 
    [sum(String.split(elf, "\n"), 0)|sum_each(rest)] 
  end  

  def sum([], sofar) do sofar end
  def sum([""], sofar) do sofar end  
  def sum([nr|rest], sofar) do 
    {nr, _} = Integer.parse(nr)
    sum(rest, sofar + nr)
  end

  
  def reverse(lst) do reverse(lst,[]) end

  def reverse([], rev) do rev end
  def reverse([a|rest], rev) do
    reverse(rest,  [a|rev])
  end

  def sort([]) do [] end
  def sort([a]) do [a] end
  def sort(lst) do
    {a, b} = split(lst)
    merge(sort(a), sort(b))
  end

  def merge([], b) do b end
  def merge(a, []) do a end
  def merge([a|rest], [b|_]=bb) when a < b do
    [a | merge(rest, bb)]
  end
  def merge(aa, [b|rest]) do
    [b | merge(aa, rest)]
  end  
  
  def split([]) do {[], []} end
  def split([a]) do {[a],[]}  end
  def split([a,b|rest]) do
    {aa,bb} = split(rest)
    {[a|aa], [b|bb]}
  end
  


  


end
