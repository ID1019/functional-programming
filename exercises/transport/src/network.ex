defmodule Network do

 def start(master, id) do
   con = spawn(fn() -> init(master, id) end)
   {:ok, con}
 end

 def init(master, id) do
   :io.format("network ~w: process ~w started ~n", [id, self()])
   receive do
     {:connect, link} ->
       :io.format("network ~w: connected to ~w~n", [id, link])
       network(master, id, link)
     :quit ->
       :ok
   end
 end

 def network(master, id, link) do
   receive do
     {:send, to, msg} ->
       IO.puts("network #{id} sending #{msg} to #{to}")
       send(link, {:send, %Netw{src: id, dst: to,  data: msg}})
       network(master, id, link)

     %Netw{dst: ^id, data: msg} ->
       IO.puts("network #{id} receiving #{msg}")
       send(master, msg)
       network(master, id, link)

     %Netw{} ->
       network(master, id, link)

     {:master, new} ->
       network(new, id, link)

     :status -> 
       :io.format("network ~w: id ~w, master: ~w, link: ~w~n", [self(), id, master, link])
       network(master, id, link)

     :quit ->
       :ok
   end
 end

end	    
			
    







