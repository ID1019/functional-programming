defmodule Nub do

  require Link
  
  def start() do
    {:ok, spawn(fn() -> init(0) end)}
  end

  def start(loss) do
    ## Loss rate is given as percent of messges lost 0-100
    {:ok, spawn(fn() -> init(loss) end)}
  end

  def init(loss) do
    :io.format("nub ~w: started~n", [self()])
    hub(loss, [])
  end

  def hub(loss, connected) do
    receive do
      {:connect, pid} ->
	:io.format("nub ~w: connecting to  ~w~n", [self(), pid])
	ref = :erlang.monitor(:process, pid)
	hub(loss, [{ref, pid}|connected])

      {:disconnect, pid} ->
	:io.format("nub ~w: disconnect  ~w~n", [self(), pid])
	:erlang.demonitor(:process, pid)
	hub(loss, List.keydelete(connected, pid, 1))

      {:DOWN, ref, :process, _, _}  ->
	:io.format("nub ~w: died ~w~n", [self(), ref])
	hub(loss, List.keydelete(connected, ref, 0))

      Link.frame() = frm ->
	if :rand.uniform(100) <= loss do
	    :io.format("nub: throwing away ~w\n", [frm])
	    :ok
	else
	  Enum.each(connected, fn({_,pid}) -> send(pid, frm) end)
	end
	hub(loss, connected)
	
      :status ->
	:io.format("nub ~w: connected to ~w~n", [self(), connected])
	hub(loss, connected)

      :quit ->
	:ok

    end
  end

end
 
