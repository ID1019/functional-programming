defmodule Dinner do

  # Start a dinner.
  def start(n) do
    seed = 1234
    dinner = spawn(fn -> init(n, seed) end)
    Process.register(dinner, :dinner)
  end

  # Start table at another node
  def start(n, node) do
    seed = 1234
    dinner = spawn(fn -> init(n, seed, node) end)
    Process.register(dinner, :dinner)
  end  
  
  # Stop the dinner.
  def stop() do
    case Process.whereis(:dinner) do
      nil ->
	:ok
      pid ->
	send(pid, :abort)
    end
  end
  
  defp init(n, seed) do
    c1 = Chopstick.start()
    c2 = Chopstick.start()
    c3 = Chopstick.start()
    c4 = Chopstick.start()
    c5 = Chopstick.start()
    ctrl = self()
    Philosopher.start(n, 5, c1, c2, "Arendt", ctrl, seed + 1)
    Philosopher.start(n, 5, c2, c3, "Hypatia", ctrl, seed + 2)
    Philosopher.start(n, 5, c3, c4, "Simone", ctrl, seed + 3)
    Philosopher.start(n, 5, c4, c5, "Elisabeth", ctrl, seed + 4)
    Philosopher.start(n, 5, c5, c1, "Ayn", ctrl, seed + 5)
    wait(5, [c1, c2, c3, c4, c5])
  end






  
  defp init(n, seed, node) do
    c1 = Chopstick.start(node)
    c2 = Chopstick.start(node)
    c3 = Chopstick.start(node)
    c4 = Chopstick.start(node)
    c5 = Chopstick.start(node)
    ctrl = self()
    Philosopher.start(n, 5, c1, c2, "Arendt", ctrl, seed + 1)
    Philosopher.start(n, 5, c2, c3, "Hypatia", ctrl, seed + 2)
    Philosopher.start(n, 5, c3, c4, "Simone", ctrl, seed + 3)
    Philosopher.start(n, 5, c4, c5, "Elisabeth", ctrl, seed + 4)
    Philosopher.start(n, 5, c5, c1, "Ayn", ctrl, seed + 5)
    wait(5, [c1, c2, c3, c4, c5])
  end
  
  
  defp wait(0, chopsticks) do
    Enum.each(chopsticks, fn(c) -> Chopstick.quit(c) end)
    Process.unregister(:dinner)
  end

  defp wait(n, chopsticks) do
    receive do
      :done ->
        wait(n - 1, chopsticks)

      :abort ->
	## in order to kill all chopsticks and philosophers
	:io.format("dinner aborted~")
        Process.exit(self(), :kill)
    end
  end

end
