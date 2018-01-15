defmodule Semaphore do

  def semaphore(0) do
    receive do
      :release ->
        semaphore(1)
    end
  end

  def semaphore(n) do
    receive do
      {:request, from} ->
        send(from, :granted)
        semaphore(n - 1)

      :release ->
        semaphore(n + 1)
    end
  end

  def request(semaphore) do
    send(semaphore, {:request, self()})

    receive do
      :granted ->
        :ok
    end
  end

  # Remove deadlock
  def request(semaphore) do
    send(semaphore, {:request, self()})

    receive do
      :granted ->
        :ok
    after
      1000 ->
        :abort
    end
  end

  # Return resource before getting it
  def request(semaphore) do
    ref = make_ref()
    send(semaphore, {:request, ref, self()})
    wait(semaphore, ref)
  end

  def wait(semaphore, ref) do
    receive do
      {:granted, ^ref} ->
        :ok

      {:granted, _} ->
        wait(semaphore, ref)
    after
      1000 ->
        send(semaphore, :release)
        :abort
    end
  end

end
