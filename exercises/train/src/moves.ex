defmodule Moves do

  def single({_, 0}, state)  do state end

  def single({:one, n}, {main, one, two}) when n > 0 do
    wgns = Train.take(main, n)
    {Train.drop(main, n), Train.append(wgns, one), two}
  end
  def single({:one, n}, {main, one, two}) when n < 0 do
    wgns = Train.take(one, -n)
    {Train.append(wgns, main), Train.drop(one, -n), two}
  end

  def single({:two, n}, {main, one, two}) when n > 0 do
    wgns = Train.take(main, n)
    {Train.drop(main, n), one, Train.append(wgns, two)}
  end
  def single({:two, n}, {main, one, two}) when n < 0 do
    wgns = Train.take(two, -n)
    {Train.append(wgns, main), one, Train.drop(two, -n)}
  end
  

  

end
