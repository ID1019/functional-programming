defmodule Log do

  ## Cut up a log of given size in pieces each cut has a price
  ## proportional to the size of the log being cut. Minimize the cost
  ## of cutting.


  ## First try, generate all different ways you can split the log.
  
  def split(seq) do split(seq, 0, [], []) end

  def split([], l, left, right)  do
    [{left, right, l}]
  end
  def split([s|rest], l, left, right) do
    split(rest, l+s, [s|left], right) ++
    split(rest, l+s, left, [s|right])
  end

  ## Avoiding mirror solutions.
  
  def splat([]) do [] end
  def splat([s]) do [s] end
  def splat([s|seq]) do splat(seq, s, [s], []) end  

  def splat([], l, left, right)  do
    [{left, right, l}]
  end  
  def splat([s], l, left, [])  do
    [{left, [s], l+s}]
  end
  def splat([s], l, [], right)  do
    [{[s], right, l+s}]
  end
  def splat([s|rest], l, left, right) do
    splat(rest, l+s, [s|left], right) ++
    splat(rest, l+s, left, [s|right])
  end


  ## This is the naive version. 
  
  def cost([]) do 0 end
  def cost([_]) do 0 end
  def cost(seq) do cost(seq, 0, [], []) end  

  def cost([], l, left, right)  do
   cost(left) + cost(right) + l
  end
  def cost([s], l, [], right)  do
    cost(right) + l + s
  end
  def cost([s], l, left, [])  do
    cost(left) + l + s
  end
  def cost([s|rest], l, left, right) do
    alt1 = cost(rest, l+s, [s|left], right)
    alt2 = cost(rest, l+s, left, [s|right])
    if alt1 < alt2 do
      alt1
    else
      alt2
    end
  end

  ## Adding a tree as return value.

  def cast([]) do {0, :na} end
  def cast([s]) do {0, s}end
  def cast(seq) do cast(seq, 0, [], []) end  

  def cast([], l, left, right)  do
    {cl, sl} = cast(left)
    {cr, sr} = cast(right)
    {cl+cr+l, {sl,sr}}
  end
  def cast([s], l, [], right)  do
    {cr, sr} = cast(right)
    {cr + l + s, {s, sr}}
  end
  def cast([s], l, left, [])  do
    {cl, sl} = cast(left)
    {cl + l + s, {sl, s}}
  end
  def cast([s|rest], l, left, right) do
    {cl, sl} = cast(rest, l+s, [s|left], right)
    {cr, sr} = cast(rest, l+s, left, [s|right])
    if cl < cr do
      {cl, sl}
    else
      {cr, sr}
    end
  end
  

  ## This is the dynamic version where we add a memory.

  def cust([]) do {0, :na} end
  def cust(seq) do
    {c, t, _mem} = cust(Enum.sort(seq), Memo.new())
    #l = length(Memo.to_list(mem))
    #IO.puts("mumber of sequences in memory: #{l}")
    {c, t}
  end
    

  def check(seq,  mem) do
    case Memo.lookup(mem, seq) do
      nil ->
	{c, t, mem} = cust(seq, mem)
	{c, t, Memo.add(mem, seq, {c,t})}
      {c, t} ->
	{c, t, mem}
    end
  end

  ## Avoid mirror solutions by adding the first element to the left.
  def cust([s], mem) do {0, s, mem}end
  def cust([s|rest], mem) do cust(rest, s, [s], [], mem) end

  
  def cust([], l, left, right, mem)  do
    {cl, sl, mem} = check(Enum.reverse(left), mem)
    {cr, sr, mem} = check(Enum.reverse(right), mem)
    {cl+cr+l, {sl,sr}, mem}
  end
  # left pile will never be empty 
  #def cust([s], l, [], right, mem)  do
  #  {cr, sr, mem} = check(Enum.reverse(right), mem)
  #  {cr + l + s, {s, sr}, mem}
  #end
  def cust([s], l, left, [], mem)  do
    {cl, sl, mem} = check(Enum.reverse(left), mem)
    {cl + l + s, {sl, s}, mem}
  end
  def cust([s|rest], l, left, right, mem) do
    {cl, sl, mem} = cust(rest, l+s, [s|left], right, mem)
    {cr, sr, mem} = cust(rest, l+s, left, [s|right], mem)
    if cl < cr do
      {cl, sl, mem}
    else
      {cr, sr, mem}
    end
  end

  
  
end
