defmodule Test do

  # Naïve lock usage
  def do_it(thing, lock) do
    case Cell.get(lock) do
      :taken ->
        do_it(thing, lock)

      :open ->
        Cell.set(lock, :taken)
        do_ya_critical_thing(thing)
        Cell.set(lock, :open)
    end
  end

  # Lock with atomic swap usage
  def do_it(thing, lock) do
    case Cell.swap(lock, :taken) do
      :taken ->
        do_it(thing, lock)

      :open ->
        do_ya_critical_thing(thing)
        Cell.set(lock, :open)
    end
  end

  # Peterson's algorithm
  def lock(id, m, p, q) do
    Cell.set(m, true)
    other = rem(id + 1, 2)
    Cell.set(q, other)

    case Cell.get(p) do
      false ->
        :locked

      true ->
        case Cell.get(q) do
          ^id ->
            :locked

          ^other ->
            lock(id, m, p, q)
        end
    end
  end

  def unlock(_id, m, _p, _q) do
    Cell.set(m, false)
  end

end
