defmodule Philosopher do

  @dream 1000
  @eat 50
  @delay 200

  def start(hunger, strength, left, right, name, ctrl, seed) do
    spawn_link(fn -> init(hunger, strength, left, right, name, ctrl, seed) end)
  end

  defp init(hunger, strength, left, right, name, ctrl, seed) do
    # gui = Gui.start(name)
    gui = nil
    :rand.seed(:exsplus, {seed, seed, seed})
    dreaming(hunger, strength, left, right, name, ctrl, gui)
  end

  defp dreaming(0, strength, _left, _right, name, ctrl, gui) do
    IO.puts("#{name} is happy, strength is still #{strength}!")
    # send gui, :stop
    send ctrl, :done
  end
  defp dreaming(hunger, 0, _left, _right, name, ctrl, gui) do
    IO.puts("#{name} is starved to death, hunger is down to #{hunger}!")
    # send gui, :stop
    send ctrl, :done
  end
  defp dreaming(hunger, strength, left, right, name, ctrl, gui) do
    IO.puts("#{name} is dreaming...")
    delay(@dream)
    waiting(hunger, strength, left, right, name, ctrl, gui)
  end

  defp waiting(hunger, strength, left, right, name, ctrl, gui) do
    # send gui, :waiting
    IO.puts("#{name} is waiting, #{hunger} to go!")

    case Chopstick.request(left) do
      :ok ->
        delay(@delay)
        case Chopstick.request(right) do
          :ok ->
            IO.puts("#{name} received both sticks!")
            eating(hunger, strength, left, right, name, ctrl, gui)
        end 
    end
  end

  defp eating(hunger, strength, left, right, name, ctrl, gui) do
    # send, gui :enter
    IO.puts("#{name} is eating...")

    delay(@eat)

    Chopstick.return(left)
    Chopstick.return(right)

    # send gui, :leave
    dreaming(hunger - 1, strength, left, right, name, ctrl, gui)
  end

  defp delay(t), do: sleep(t)

  defp sleep(0), do: :ok
  defp sleep(t), do: :timer.sleep(:rand.uniform(t))

end