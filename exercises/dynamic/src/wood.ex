defmodule Test do

  ## A specification is given as [{number, lengt} ...]

  ## Calulate the minimal cost.
  
  
  def cost([]) do {0, :na} end

  def cost([c]) do single(c) end

  def cost(seq) do cost(seq, 0, [], []) end

  ## cost(seq, l, left, right) :
  ##
  ##     seq : remaining elements
  ##       l : length of segments in left and right
  ##    left : elements to the left
  ##   right : elements to the right

  def cost([], l, left, right)  do
    ## The left and right contains at least one element each. It's
    ## safe to do  recursive calls. 
    cost(left) + cost(right) + l
  end

  ## The last element needs special treatment in order to not go into
  ## a loop. All segments can not go into either left nor right.

  ## Two sinple cases when the element is a single segment.
  def cost([{1,s}], l, [], right) do cost(right)  + l + s end
  def cost([{1,s}], l, left,  []) do cost(left)  + l + s end

  ## The more general case, divide the segment into all possible
  ## segments. Make sure that at least one element goes into the empty
  ## part.

  def cost([{n,s}=c], l, [], right) do  
    List.foldl(
      [cost([c]) + cost(right) + l+(n*s), 
       Enum.map(alternatives(n, s),
	 fn({lft,rgt}) ->
	   cost([lft]) + cost([rgt|right]) + n*s + l
	 end)
       ],
      :inf,
      fn(x,a) -> if x < a do x else a end end
    )
  end
  def cost([{n,s}=c], l, left, []) do
    List.foldl(
      [cost(left) + cost([c]) +  l+(n*s),
       Enum.map(alternatives(n, s),
	 fn({lft,rgt}) ->
	   cost([lft|left]) + cost([rgt]) + n*s + l
	 end)
       ],
      :inf,
      fn(x,a) -> if x < a do x else a end end
    )
  end

  ## Special case when there is only one element. We should either
  ## place this to the left or to the right.
  
  def cost([{1,s}=c|rest], l, left, right) do
    alt1 = cost(rest, l+s, [c|left], right)
    alt2 = cost(rest, l+s, left, [c|right])
    if alt1 < alt2 do
      alt1
    else
      alt2
    end    
  end

  ## The general case, this is where we split the segments in all
  ## possible ways and select the lowest cost. 

  def cost([{n,s}=c|rest], l, left, right) when n > 1 do
    List.foldl([cost(rest, l+(n*s), [c|left], right),
		cost(rest, l+(n*s), left, [c|right]),      
		Enum.map(alternatives(n, s),
		  fn({lft,rgt}) ->
		    cost(rest, l+(n*s), [lft|left], [rgt|right]) 
		  end)
	       ],
      :inf,
      fn(x,a) -> if x < a do x else a end end
    )
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
