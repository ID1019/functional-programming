defmodule Wood do

  ## A specification is given as [{number, lengt} ...]

  ## Calulate the minimal cost.

  def cost(spec) do
    spec = Enum.sort(spec, fn({_,s1}, {_,s2}) -> s1 < s2 end)
    {answ, _} = cost(spec, Map.new())
    answ
  end

  def check(spec, mem) do
    case Map.get(mem, spec) do
      :nil ->
	{answ, mem} = cost(spec, mem)
	{answ, Map.put(mem, spec, answ)}
      answ ->
	{answ, mem}
    end
  end

  
  def cost([], mem) do {{0, :na},mem} end
  def cost([c], mem) do {single(c), mem} end
  def cost(seq, mem) do cost(seq, 0, [], [], mem) end

  ## cost(seq, l, left, right) :
  ##
  ##     seq : remaining elements
  ##       l : length of segments in left and right
  ##    left : elements to the left
  ##   right : elements to the right

  def cost([], l, left, right, mem)  do
    ## The left and right contains at least one element each. It's
    ## safe to do  recursive calls. 
    {c1,mem} = check(Enum.reverse(left), mem)
    {c2,mem} = check(Enum.reverse(right), mem)
    {c1+c2+l, mem}
  end

  ## The last element needs special treatment in order to not go into
  ## a loop. All segments can not go into either left nor right.

  ## Two simple cases when the element is a single segment.
  def cost([{1,s}], l, [], right, mem) do
    cost([], l+s, [{1,s}], right, mem)
  end
  def cost([{1,s}], l, left, [], mem) do
    cost([], l+s, left, [{1,s}], mem)
  end

  ## The more general case, divide the segment into all possible
  ## segments. Make sure that at least one element goes into the empty
  ## part.

  def cost([{n,s}=c], l, [], right, mem) do  
    Enum.reduce(
      [{[c],right, l+(n*s)} |
       Enum.map(alternatives(n, s), 
	 fn({lft,rgt}) ->
	   {[lft], [rgt|right], n*s + l}
	 end)
       ],
      {:inf, mem},
      fn({lft, rgt, l}, {acc, mem}) ->
	{c1, mem} = check(Enum.reverse(lft), mem)
	{c2, mem} = check(Enum.reverse(rgt), mem)
	c = c1+c2+l
	if (c < acc) do
	  {c, mem}
	else
	  {acc, mem}
	end
      end)
  end

  def cost([{n,s}=c], l, left, [], mem) do  
    Enum.reduce(
      [{left, [c], l+(n*s)} |
       Enum.map(alternatives(n, s), 
	 fn({lft,rgt}) ->
	   {[lft|left], [rgt], n*s + l}
	 end)
       ],
      {:inf, mem},
      fn({lft, rgt, l}, {acc, mem}) ->
	{c1, mem} = check(Enum.reverse(lft), mem)
	{c2, mem} = check(Enum.reverse(rgt), mem)
	c = c1+c2+l
	if (c < acc) do
	  {c, mem}
	else
	  {acc, mem}
	end
      end)
  end
  
  ## Special case when there is only one element. We should either
  ## place this to the left or to the right.
  
  def cost([{1,s}=c|rest], l, left, right, mem) do
    {alt1,mem} = cost(rest, l+s, [c|left], right, mem)
    {alt2,mem} = cost(rest, l+s, left, [c|right], mem)
    if alt1 < alt2 do
      {alt1, mem}
    else
      {alt2, mem}
    end    
  end

  ## The general case, this is where we split the segments in all
  ## possible ways and select the lowest cost. 

  def cost([{n,s}=c|rest], l, left, right, mem) when n > 1 do
    Enum.reduce(
      [{left, [c|right], l+(n*s)},
       {[c|left],right, l+(n*s)} |
       Enum.map(alternatives(n, s), 
	 fn({lft,rgt}) ->
	   {[lft|left], [rgt], n*s + l}
	 end)
       ],
      {:inf, mem},
      fn({lft, rgt, l}, {acc, mem}) ->
	{c, mem} = cost(rest, l, lft, rgt, mem)
	if c < acc do
	  {c, mem}
	else
	  {acc, mem}
	end
      end)
  end

  ## The cost of a single node sequence. We could have called the more
  ## general cost/1 but since we know that we are dealing with a
  ## single segment we call singe/1 directly.

  def single({n,s}) do single(n,s) end

  def single(1, _) do 0 end  
  def single(n, s) do
    List.foldl(
      Enum.map(alternatives(n,s), fn({l,r}) -> single(l) + single(r) + n * s end),
      :inf,
      fn(x,a) -> if x < a do x else a end end
    )
  end
  
  ## Generate all possible ways to divide this, apart from the two
  ## cases where either part is empty. 
  def alternatives(n,s) when n > 1 do
    for i <- 1..(n-1) do {{n-i,s}, {i, s}} end
  end


  ## Only here to show how to divide a specification. 

  def split([]) do [] end
  def split(seq) do split(seq, 0, [], []) end

  def split([], l, left, right)  do
    [{left, right, l}]
  end
  def split([{1,s}|rest], l, left, right) do
    split(rest, l+s, [s|left], right) ++
    split(rest, l+s, left, [s|right])
  end
  def split([{n,s}|rest], l, left, right) do
    all = for i <- 1..(n-1) do {{n-i,l}, {i, l}} end
    List.foldr(all, [], fn({lft,rgt}, a) ->  split(rest, l+(n*s), [lft|left], [rgt|right]) ++ a end)
  end
  
  
end
