defmodule Test do

  def test() do
    sorter = Bitonic.start(8)
    send(sorter, {:sort, [7,3,2,4,6,5,1,8], :foo, self()})
    receive do
      {:sorted, :foo, sorted} ->
	sorted
      :done ->
	:ok
    end
  end
  

  
end
