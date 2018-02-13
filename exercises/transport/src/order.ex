defmodule Order do


  def start(master, to) do
    {:ok, spawn(fn() -> init(master, to) end)}
  end

  def init(master, to) do
    receive do
      {:connect, netw} ->
	order(master, to, 0, 0, [], netw)
    end
  end

  def order(master, to, n, i, [], netw) do
    receive do
	
      %Dgr{id: i, data: msg} ->
	send(netw, {:send, to, %Ack{id: i}})
	send(master, msg)
	order(master, to, n, i+1, [], netw)
	
     %Dgr{id: j} when j < i ->
	send(netw, {:send, to, %Ack{id: j}})
	order(Master, to, N, I, [], Net)

      %Ack{} ->
	order(master, to, n, I, [], netw)
      {:send, msg} ->
	send(netw, {:send, to, %Dgr{id: n, data: msg}})
	order(master, to, n+1, I, [{n,Msg}], netw);
      {:master, new} ->
	order(new, to, n, I, [], netw)
    end
  end
  def order(master, to, n, i, [{a,res}|rest]=buffer, netw) do
    receive do

      %Dgr{id: i, data: msg} ->
	send(netw, {:send, to, %Ack{id: i}})
	send(master, msg)
	order(master, to, n, i+1, buffer, netw)

      %Dgr{id: j} when j < i ->
	send(netw, {:send, to, %Ack{id: j}})	    
	order(master, to, n, i, buffer, netw)

      %Ack{id: ^a} ->
	order(master, to, n, i, rest, netw)

      %Ack{id: b} when b < a ->
	order(master, to, n, I, Buffer, netw);

      {:send, msg} ->
	send(netw,{:send, to, %Dgr{id: n, data: msg}})
	order(master, to, n+1, i, buffer++[{n,msg}], netw)

      {:master, new} ->
	order(new, to, n, i, buffer, netw)
		
    after 10 ->
	dgr = %Dgr{id: a, data: res}
	:io.format("order re-sending ~w~n", [dgr])
	send(netw, {:send, to, dgr})
	order(master, to, n, i, buffer, netw)
    end
  end
end





    
    



	    
			  
	    
    
