defmodule Memory do

  def new(values) do
    pids = Enum.map(values, fn(v) -> Cell.start(v) end)
  end

  def set(array, n, v) do
    Cell.set(Enum.at(array, n), v)
  end

  def get(array, n) do
    Cell.get(Enum.at(array, n))
  end

  def get_asyn(array, n) do
    Cell.get_asyn(Enum.at(array, n))
  end

  def get_answ(ref) do
    Cell.get_answ(ref)
  end

  def sum(array) do
    ids = 0..(length(array) - 1)
    refs = Enum.map(ids, fn(i) -> get_asyn(array, i) end)
    List.foldl(refs, 0, fn(r, a) -> {:ok, v} = get_answ(r); a + v end)
  end

end