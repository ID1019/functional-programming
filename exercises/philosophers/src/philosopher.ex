defmodule Philosopher do

  @dream 800
  @eat 100
  @delay 0

  @timeout 1000

  # Create a new philosopher process.
  def start(hunger, strength, left, right, name, ctrl, seed) do
    spawn_link(fn -> init(hunger, strength, left, right, name, ctrl, seed) end)
  end

  defp init(hunger, strength, left, right, name, ctrl, seed) do
    gui = Gui.start(name)
    :rand.seed(:exsss, {seed, seed, seed})
    dreaming(hunger, strength, left, right, name, ctrl, gui)
  end

  # Philosopher is in a dreaming state.
  defp dreaming(0, strength, _left, _right, name, ctrl, gui) do
    IO.puts("#{name} is happy, strength is still #{strength}!")
    send(gui, :stop)
    send(ctrl, :done)
  end
  defp dreaming(hunger, 0, _left, _right, name, ctrl, gui) do
    IO.puts("#{name} is starved to death, hunger is down to #{hunger}!")
    send(gui, :stop)
    send(ctrl, :done)
  end
  defp dreaming(hunger, strength, left, right, name, ctrl, gui) do
    IO.puts("#{name} is dreaming!")
    send(gui, :leave)

    ##  this is where we sleep
    delay(@dream)

    IO.puts("#{name} wakes up")
    waiting(hunger, strength, left, right, name, ctrl, gui)
  end

  # Philosopher is waiting for chopsticks.
  defp waiting(hunger, strength, left, right, name, ctrl, gui) do
    send(gui, :waiting)
    IO.puts("#{name} is waiting, #{hunger} to go!")

    case Chopstick.request(left) do
      :ok ->
	IO.puts("#{name} received left stick")
        delay(@delay)

        case Chopstick.request(right) do
          :ok ->
            IO.puts("#{name} received both sticks!")
            eating(hunger, strength, left, right, name, ctrl, gui)
        end

    end
  end

  # Philosopher is eating.
  defp eating(hunger, strength, left, right, name, ctrl, gui) do
    send(gui, :enter)
    #IO.puts("#{name} is eating...")

    delay(@eat)

    Chopstick.return(left)
    Chopstick.return(right)

    dreaming(hunger - 1, strength, left, right, name, ctrl, gui)
  end

  defp delay(t), do: sleep(t)

  defp sleep(0), do: :ok
  defp sleep(t), do: :timer.sleep(:rand.uniform(t))

end
