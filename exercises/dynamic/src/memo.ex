defmodule Memo do

  #def new() do %{} end

  #def add(mem, key, val) do Map.put(mem, key, val) end
  #def add(mem, key, val) do Map.put(mem, :binary.list_to_bin(key), val) end  

  #def lookup(mem, key) do  Map.get(mem, key) end
  #def lookup(mem, key) do  Map.get(mem, :binary.list_to_bin(key)) end




  def new() do [] end

  def add(mem, [n], val) do insert(mem, n, val) end
  def add(mem, [n|ns], val) do add(mem, n, ns, val) end

  def add([], n, rest, value) do [{n, nil, add([], rest, value)}] end
  def add([{n, val, sub}|mem], n, rest, value) do [{n, val, add(sub, rest, value)}|mem] end  
  def add([first|mem], n, rest, value) do [first| add(mem, n, rest, value)] end  				   

  def insert([], n, val) do [{n, val, []}] end
  def insert([{n, nil, sub}|mem], n, val) do [{n, val, sub}|mem] end  
  def insert([first|mem], n, val) do [first| insert(mem, n, val)] end  

  def lookup([], _) do nil end
  def lookup(mem, [n]) do val(mem, n) end
  def lookup(mem, [n|ns]) do lookup(mem, n, ns) end

  def lookup([], _, _) do nil end
  def lookup([{n, _, sub}|_], n, ns) do lookup(sub, ns) end
  def lookup([_|mem], n, ns) do lookup(mem, n, ns) end    

  def val([], _) do nil end
  def val([{n, val, _}|_], n) do val end
  def val([_|rest], n) do val(rest, n) end    


  def to_list(mem) do to_list(mem,[]) end
  
  def to_list([], _) do [] end
  def to_list([{n,nil,sub}|rest], seq) do
    to_list(sub, [n|seq]) ++ to_list(rest, seq)
  end
  def to_list([{n,val,sub}|rest], seq) do
      [{Enum.reverse([n|seq]), val} | to_list(sub, [n|seq])] ++  to_list(rest, seq)end
  

end


