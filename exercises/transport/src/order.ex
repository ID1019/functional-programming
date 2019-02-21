defmodule Order do

  require Record

  Record.defrecord(:ord, seq: 0, data: nil)

  Record.defrecord(:ack, seq: 0)
  Record.defrecord(:dgr, seq: 0, data: [])
  
  def start(master, to) do
    {:ok, spawn(fn() -> init(master, to) end)}
  end

  def init(master, to) do
   :io.format("order to ~w: process ~w started~n", [to, self()])
    receive do
      {:connect, netw} ->
	:io.format("order to ~w: connected to ~w~n", [to, netw])
	order(master, to, 0, 0, [], netw)
    end
  end

  def order(master, to, n, i, [], netw) do
    receive do
	
      ord(seq: ^i, data: msg) ->
	send(netw, {:send, to, ack(seq: i)})
	send(master, msg)
	order(master, to, n, i+1, [], netw)
	
     ord(seq: j) when j < i ->
	send(netw, {:send, to, ack(seq: j)})
	order(master, to, n, i, [], netw)

      ack() ->
	order(master, to, n, i, [], netw)

      {:send, msg} ->
	send(netw, {:send, to, ord(seq: n, data: msg)})
	order(master, to, n+1, i, [{n,msg}], netw);

      {:master, new} ->
	order(new, to, n, i, [], netw)
    end
  end
  def order(master, to, n, i, [{a,res}|rest]=buffer, netw) do
    receive do

      ord(seq: ^i, data: msg) ->
	send(netw, {:send, to, ack(seq: i)})
	send(master, msg)
	order(master, to, n, i+1, buffer, netw)

      ord(seq: j) when j < i ->
	send(netw, {:send, to, ack(seq: j)})
	order(master, to, n, i, buffer, netw)

      ack(seq: ^a) ->
	order(master, to, n, i, rest, netw)

      ack(seq: b) when b < a ->
	order(master, to, n, i, buffer, netw);

      {:send, msg} ->
	send(netw,{:send, to, ord(seq: n, data: msg)})
	order(master, to, n+1, i, buffer++[{n,msg}], netw)

      {:master, new} ->
	order(new, to, n, i, buffer, netw)
		
    after 10 ->
	dgr = ord(seq: a, data: res)
	##:io.format("order to ~w resending ~w\", [to, dgr])
	send(netw, {:send, to, dgr})
	order(master, to, n, i, buffer, netw)
    end
  end
end





    
    



	    
			  
	    
    
