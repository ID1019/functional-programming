defmodule Test do

  def test_flow(n, size) do
    sender = spawn(fn() -> connect(fn(net) ->  sender_flow(n, net) end) end)
    receiver = spawn(fn() ->  connect(fn(net) -> receiver_flow(n, net) end) end)
    {:ok, ls} = flow(sender, size, 2, 1)
    {:ok, lr} = flow(receiver, size, 1, 2)
    switch(ls, lr)
  end


  def test_flow(n, size, loss) do
    sender = spawn(fn() -> connect(fn(net) ->  sender_flow(n, net) end) end)
    receiver = spawn(fn() ->  connect(fn(net) -> receiver_flow(n, net) end) end)
    {:ok, ls} = flow(sender, size, 2, 1)
    {:ok, lr} = flow(receiver, size, 1, 2)
    connect(ls, lr, loss)
  end


  def test_order(n) do
    sender =  spawn(fn() -> connect(fn(net) -> sender_order(n, net) end) end)
    receiver = spawn(fn() ->  connect(fn(net) -> receiver_order(n, net) end) end)
    {:ok, ls} = order(sender, 2, 1)
    {:ok, lr} = order(receiver, 1, 2)
    switch(ls, lr)
  end

  def test_order(n, loss) do
    sender =  spawn(fn() -> connect(fn(net) -> sender_order(n, net) end) end)
    receiver = spawn(fn() ->  connect(fn(net) -> receiver_order(n, net) end) end)
    {:ok, ls} = order(sender, 2, 1)
    {:ok, lr} = order(receiver, 1, 2)
    connect(ls, lr, loss)
  end

  def test_switch(n) do
    sender =  spawn(fn() -> connect(fn(net) -> sender_netw(n, 2, net) end) end)
    receiver =  spawn(fn() -> connect(fn(net) -> receiver_netw(n, 1, net) end) end)
    {:ok, ls} = netw(sender, 1)
    {:ok, lr} = netw(receiver, 2)
    switch(ls, lr)
  end


  def test_netw(n, loss) do
    sender =  spawn(fn() -> connect(fn(net) ->  sender_netw(n, 2, net) end) end)
    receiver =  spawn(fn() ->  connect(fn(net) -> receiver_netw(n, 1, net) end) end)
    {:ok, ls} = netw(sender, 1)
    {:ok, lr} = netw(receiver, 2)
    connect(ls, lr, loss)
    :ok
  end

  def test_link(n) do
    sender = spawn(fn() -> connect(fn(lnk) -> sender_link(n, lnk) end) end)
    receiver = spawn(fn() -> connect(fn(lnk) -> receiver_link(n, lnk) end) end)
    {:ok, ls} = Link.start(sender)
    {:ok, lr} = Link.start(receiver)
    send(ls,{:connect, lr})
    send(lr, {:connect, ls})
    send(sender, {:connect, ls})
    send(receiver, {:connect, lr})
    :ok
  end

  ## connecting layers together
  
  def flow(app, size, to, i) do
    {:ok, flow} = Flow.start(size)    
    send(app, {:connect, flow})
    order(flow, to, i)
  end

  def order(app, to, i) do
    {:ok, order} = Order.start(app, to)    
    send(app, {:connect, order})
    netw(order, i)
  end

  def netw(app, i) do
    {:ok, netw} = Network.start(app, i)
    send(app, {:connect, netw})
    lnk(netw)
  end

  def lnk(app) do
    {:ok, link} = Link.start(app)
    send(app, {:connect, link})
    {:ok, link}
  end


  ## adding a switch and connecting it to two links
  
  def switch(l1, l2) do
    {:ok, switch} = Switch.start()
    {:ok, s1} = Switch.new(switch)
    {:ok, s2} = Switch.new(switch)
    send(l1, {:connect, s1})
    send(s1, {:connect, l1})
    send(l2, {:connect, s2})
    send(s2, {:connect, l2})
  end

  ## adding a lossy hub and connecting it to two links  
  
  def connect(l1, l2, loss) do
    {:ok, hub} = Nub.start(loss)
    send(l1, {:connect, hub})
    send(l2, {:connect, hub})
    send(hub, {:connect, l1})
    send(hub, {:connect, l2})
  end

  ## wait for a connection 
  
  def connect(f) do
    receive do
	{:connect, n} ->
	    f.(n)
    end
  end

  ## test processes
  
  def sender_flow(0, _) do :ok end
  def sender_flow(i, n) do
    :io.format("sender sending ~w\n", [i])
    send(n, {:send, "message #{i}" , self()})
    receive  do
      :ok ->
	sender_flow(i-1, n)
    end
  end

  def receiver_flow(0,_) do :ok end
  def receiver_flow(i,n) do
    send(n, {:read, 1, self()})
    receive do
      {:ok, l, msg} ->
	:io.format("receiver received ~w\n", [msg])
	receiver_flow(i-l,n)
    end
  end

  def sender_order(0, _) do  :ok end
  def sender_order(i, n) do
    IO.puts("sender sending #{i} ")
    send(n, {:send, i})
    sender_order(i-1, n)
  end

  def receiver_order(0,_) do :ok end
  def receiver_order(i,n) do
    receive do
      msg ->
	:io.format("receiver received ~w\n", [msg])
	receiver_order(i-1,n)
    end
  end

  def sender_netw(0, _, _) do :ok end
  def sender_netw(i, t, n) do
    :io.format("sender sending ~w\n", [i])
    send(n, {:send, t, i})
    sender_netw(i-1, t, n)
  end

  def receiver_netw(0, _, _) do :ok end
  def receiver_netw(i, t, n) do
    receive do
      msg ->
	:io.format("receiver received ~w\n", [msg])
	receiver_netw(i-1, t, n)
    end
  end

  def sender_link(0, _) do :ok end
  def sender_link(i, n) do
    IO.puts("sender sending #{i} ")
    send(n, {:send, i})
    sender_link(i-1, n)
  end

  def receiver_link(0, _) do :ok end
  def receiver_link(i, n) do
    receive do
      msg ->
	:io.format("receiver received ~w\n", [msg])
	receiver_link(i-1, n)
    end
  end
  
  
end

