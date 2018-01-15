defmodule Memory do

  # Create new memory cellwith given values.
  def new(values) do
    pids = Enum.map(values, fn(v) -> Cell.start(v) end)
  end

  # Set a specific cell in the memory.
  # Usage: set(array, position, value)
  def set(array, n, v) do
    Cell.set(Enum.at(array, n), v)
  end

  # Get the value of a specific cell in the memory.
  # Usage: get(array, position)
  def get(array, n) do
    Cell.get(Enum.at(array, n))
  end

  # Query the memory for the value of a specific cell and
  # obtain a reference to use to retrieve such value
  # asynchronously with get_answ/1.
  # Usage: get_asyn(array, position)
  def get_asyn(array, n) do
    Cell.get_asyn(Enum.at(array, n))
  end

  # Retrieve a value given a reference to an asynchronous
  # request.
  def get_answ(ref) do
    Cell.get_answ(ref)
  end

  # Sum all the values contained in the cells of the memory. 
  def sum(array) do
    ids = 0..(length(array) - 1)
    refs = Enum.map(ids, fn(i) -> get_asyn(array, i) end)
    List.foldl(refs, 0, fn(r, a) -> {:ok, v} = get_answ(r); a + v end)
  end
end
