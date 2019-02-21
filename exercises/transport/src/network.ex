defmodule Network do

  require Record

  Record.defrecord(:netw, src: 0, dst: 0, data: nil)

  
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
       ##:io.format("network ~w sending ~w to ~w\n", [id, msg, to])
       send(link, {:send, netw(src: id, dst: to,  data: msg)})
       network(master, id, link)

     netw(dst: ^id, data: msg) ->
       ##:io.puts("network ~w receiving ~w\n", [id, msg])
       send(master, msg)
       network(master, id, link)

     netw() ->
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
			
    







