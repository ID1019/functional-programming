defmodule Switch do

  require Network
  
  def start() do
    {:ok, spawn(fn() -> init() end)}
  end

  def new(switch) do
    send(switch, {:new, self()})
    receive do
      {:ok, lnk} ->
	{:ok, lnk}
    end
  end


  def init() do
    switch([], [])
  end

  ## Hmm, how do we model a switch? It should learn from the incomming
  ## packets. Hmm, ...

  def switch(cache, all) do
    receive do
    
      {:new, net} ->
	self = self()
	con = spawn_link(fn()-> connection(self) end)
	{:ok, lnk} = Link.start(con)
	send(con, {:connect, lnk})
	send(net, {:ok, lnk})
	:io.format("switch: new link ~w~n", [lnk])
	switch(cache, [con|all])

      {:frw, src, dst, from, msg} ->
	case List.keyfind(cache, dst, 0) do
	  {^dst, to} ->
	    :io.formats("switch: forward msg ~w\n", [msg])
	    send(to, {:frw, msg})
	  nil ->
	    :io.format("switch: broadcast ~w\n", [msg])
 	    Enum.each(all, fn(cn) -> send(cn, {:frw, msg}) end)
	end
	rest = List.keydelete(cache, src, 0)
	switch([{src, from}|rest], all)

      :status ->
	:io.format("switch cache: ~w~n", [cache])
	switch(cache, all)

      :quit ->
 	Enum.each(all, fn(cn) -> send(cn, :quit) end)
	:ok
    end
  end
 
  def connection(switch) do
    receive do
      {:connect, lnk} ->
	:io.format("switch  connection connected to: ~w~n", [lnk])
	connection(switch, lnk)
    end
  end
		  
  def connection(switch, lnk) do
    receive do

      Network.netw(src: src, dst: dst)=msg ->
	##:io.format("switch connection received: ~w~n", [msg])
	send(switch, {:frw, src, dst, self(), msg})
	connection(switch, lnk)
      
      {:frw, msg} ->
	##:io.format("switch connection deliver: ~w~n", [msg])
	send(lnk, {:send, msg})
	connection(switch, lnk)	    

      :quit ->
        send(lnk, :quit)	
	:ok
    end
  end
end
    
	    
		
    
