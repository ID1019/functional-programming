defmodule Moves do


  def sequence([], state) do [state] end
  def sequence([move|rest], state) do 
    [state | sequence(rest, single(move, state))] 
  end  
  
  def single({_, 0}, state)  do state end

  def single({:one, n}, {main, one, two}) when n > 0 do
    {0, remain, wgns} = Train.main(main, n)
    {remain, Train.append(wgns, one), two}
  end
  def single({:one, n}, {main, one, two}) when n < 0 do
    wgns = Train.take(one, -n)
    {Train.append(main, wgns), Train.drop(one, -n), two}
  end

  def single({:two, n}, {main, one, two}) when n > 0 do
    {0, remain, wgns} = Train.main(main,n)
    {remain, one, Train.append(wgns, two)}
  end
  def single({:two, n}, {main, one, two}) when n < 0 do
    wgns = Train.take(two, -n)
    {Train.append(main, wgns), one, Train.drop(two, -n)}
  end


  


end
