defmodule Dynamic do

  def search(seq) do
    elem(search(seq, Memo.new()), 0)
  end


  def check(seq, mem) do
    ## key = :binary.list_to_bin(seq)
    key = seq
    case Memo.get(mem, key) do
      nil ->
	{cost, mem} = search(seq, mem)
	{cost, Memo.put(mem, key, cost)}
      cost ->
	{cost, mem}
    end
  end
  

  def search([], mem) do {0, mem} end
  def search([_], mem) do {0, mem} end
  def search([x,y], mem) do {x+y, mem} end

  def search(seq, mem) do
    Enum.reduce(split(seq), {:inf, mem}, fn({left, right, length}, {min, mem}) ->
      {cost_left, mem} =  check(left, mem) 
      {cost_right, mem} = check(right, mem)
      cost = cost_left + cost_right + length
      if cost < min do
	{cost, mem}
      else
	{min, mem}
      end
    end)
  end

  def split([x|seq]) do split(seq, x, [x], []) end

  def split([], l, left, right)  do
    [{left, right, l}]
  end
  def split([x], l, [], right)  do
    [{[x], right, l+x}]
  end
  def split([x], l, left, [])  do
    [{left, [x], l+x}]
  end
  def split([s|rest], l, left, right) do
    split(rest, l+s, add(left,s), right) ++
    split(rest, l+s, left, add(right,s))
  end
  

  def add([],x) do [x] end
  def add([h|_]=seq,x) when h > x do [x|seq] end  
  def add([h|t],x) do [h | add(t,x)] end    

end

