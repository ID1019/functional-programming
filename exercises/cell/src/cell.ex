defmodule Cell do

  # Create a new memory cell process.
  def start(n), do: spawn(fn -> cell(n) end)

  # Set the internal value of a given cell.
  def set(pid, value) do
    send(pid, {:set, value})
    :ok
  end

  # Get the internal value of a given cell.
  def get(pid) do
    send(pid, {:get, self()})

    receive do
      {:ok, value} ->
        {:ok, value}
    end
  end

  # Get the internal value of a given cell with an
  # asynchronous approach.
  def get_asyn(pid) do
    ref = make_ref()
    send(pid, {:get, ref, self()})
    ref
  end

  # Retrieve value after asynchronous approach.
  def get_answ(ref) do
    receive do
      {:ok, ref, value} ->
        {:ok, value}
    end
  end

  # Representation of a memory cell as a process.
  defp cell(n) do
    receive do
      {:set, value} ->
        cell(value)

      {:get, pid} ->
        send(pid, {:ok, n})
        cell(n)

      {:get, ref, pid} ->
        send(pid, {:ok, ref, n})
        cell(n)
    end
  end
end
