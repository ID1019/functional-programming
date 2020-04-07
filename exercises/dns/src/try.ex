defmodule Try do

  @server {8,8,8,8}
  @port 53
  @local 5300

  def start() do
    start(@local, @server, @port)
  end

  def start(local, server, port) do
    spawn(fn() -> init(local, server, port) end)
  end





  
  def init(local, server, port) do
    case :gen_udp.open(local, [{:active, true}, :binary]) do
      {:ok, local} ->
        case :gen_udp.open(0, [{:active, true}, :binary]) do
          {:ok, remote} ->
            dns(local, remote, server, port)
	    :gen_udp.close(local)
	    :gen_udp.close(remote)
          error ->
	    :gen_udp.close(local)
            :io.format("DNS error opening remote socket: ~w~n", [error])
        end
      error ->
        :io.format("DNS error opening local socket: ~w~n", [error])
    end
  end




  
  def dns(local, remote, server, port) do
    receive do
      {:udp, ^local, client, client_port, query} ->
	:io.format("request: ~w:~w~n", [client, client_port])
        <<id::16, _flags::16, qn::16, an::16, nn::16, _::16, _body::binary>> = query
	:io.format("query: ~w ~w ~w ~w ~n", [id, qn, an, nn])
        :gen_udp.send(remote, server, port, query)
        dns(local, remote, server, port)

      {:udp, ^remote, from_server, from_port, reply} ->
	:io.format("reply: ~w:~w~n", [from_server, from_port])
        <<id::16, _flags::16, qn::16, an::16, nn::16, _::16, _body::binary>> = reply
	:io.format("answer: ~w ~w ~w ~w ~n", [id, qn, an, nn])	
	
        dns(local, remote, server, port)	
      :stop ->
	:io.format("by bye~n", [])
        :ok

      :update ->
	:io.format("updating~n",[])
        Try.dns(local, remote, server, port)

      strange ->
        :io.format("strange message ~w~n", [strange])
        dns(local, remote, server, port)
    end
  end
    


end

  
