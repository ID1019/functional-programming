defmodule Order do


  def start(master, to) do
    {:ok, spawn(fn() -> init(master, to) end)}
  end

  def init(master, to) do
   :io.format("order to ~w: process ~w started~n", [to, self()])
    receive do
      {:connect, netw} ->
	:io.format("order to ~w: connected to ~w", [to, netw])
	order(master, to, 0, 0, [], netw)
    end
  end

  def order(master, to, n, i, [], netw) do
    receive do
	
      %Ord{seq: i, data: msg} ->
	send(netw, {:send, to, %Ack{seq: i}})
	send(master, msg)
	order(master, to, n, i+1, [], netw)
	
     %Ord{seq: j} when j < i ->
	send(netw, {:send, to, %Ack{seq: j}})
	order(Master, to, N, I, [], Net)

      %Ack{} ->
	order(master, to, n, I, [], netw)

      {:send, msg} ->
	send(netw, {:send, to, %Ord{seq: n, data: msg}})
	order(master, to, n+1, I, [{n,Msg}], netw);

      {:master, new} ->
	order(new, to, n, I, [], netw)
    end
  end
  def order(master, to, n, i, [{a,res}|rest]=buffer, netw) do
    receive do

      %Ord{seq: i, data: msg} ->
	send(netw, {:send, to, %Ack{seq: i}})
	send(master, msg)
	order(master, to, n, i+1, buffer, netw)

      %Ord{seq: j} when j < i ->
	send(netw, {:send, to, %Ack{seq: j}})	    
	order(master, to, n, i, buffer, netw)

      %Ack{seq: ^a} ->
	order(master, to, n, i, rest, netw)

      %Ack{seq: b} when b < a ->
	order(master, to, n, i, buffer, netw);

      {:send, msg} ->
	send(netw,{:send, to, %Ord{seq: n, data: msg}})
	order(master, to, n+1, i, buffer++[{n,msg}], netw)

      {:master, new} ->
	order(new, to, n, i, buffer, netw)
		
    after 10 ->
	dgr = %Ord{seq: a, data: res}
	IO.puts("order to #{to} resending #{dgr}")
	send(netw, {:send, to, dgr})
	order(master, to, n, i, buffer, netw)
    end
  end
end





    
    



	    
			  
	    
    
