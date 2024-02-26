defmodule Dinner do

  def start(n) do
    spawn(fn() -> init(n) end)
  end
  

  def init(n) do
    c1 = Chop.start()
    c2 = Chop.start()  
    c3 = Chop.start()
    c4 = Chop.start()
    c5 = Chop.start()
    ctrl = self()
    t0 = :erlang.timestamp()
    Phil.start(:arendt, c1, c2, n, ctrl)
    Phil.start( :hypatia, c2, c3, n, ctrl)
    Phil.start(:simone, c3, c4, n, ctrl)
    Phil.start(:elisabeth, c4, c5, n, ctrl)
    Phil.start(:ayn, c5, c1, n, ctrl)
    wait(5, t0)
  end

  def wait(0, t0) do
    t1 = :erlang.timestamp()
    IO.puts(" done in #{div(:timer.now_diff(t1,t0), 1000)} ms")
    Process.exit(self(), :kill)
  end
  def wait(n, t0) do
    receive do
      :done ->
	wait(n-1, t0)
      :abort ->
	IO.puts(" abort ")
        Process.exit(self(), :kill)
    end
  end
  
	
  end
  

				   
