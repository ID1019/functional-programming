defmodule DNS do

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
            dns(local, remote, server, port, 0, Map.new())
	  error ->
            :io.format("DNS error opening remote socket: ~w~n", [error])
	end
      error ->
        :io.format("DNS error opening local socket: ~w~n", [error])
    end
  end

  def dns(local, remote, server, server_port, n, queries) do

    receive do
      {:udp, ^local, client, client_port, msg} ->
	:io.format("message from  ~w:~w~n", [client, client_port])
	{id, _qr, _op, _aa, _tc, _rd, _ra, _resp, body} = Msg.decode(msg)
	:io.format("id: ~w, body: ~p~n", [id, body])
	query = Msg.fake(msg, n)
	:gen_udp.send(remote, server, server_port, query)
	dns(local, remote, server, server_port, n+1, Map.put(queries, n, {id, client, client_port}))

      {:udp, ^remote, server, server_port, msg} ->
	:io.format("reply from  ~w:~w~n", [server, server_port])
	{id, _qr, _op, _aa, _tc, _rd, _ra, _resp, reply} = Msg.decode(msg)
	:io.format("id: ~w, reply: ~p~n", [id, reply])
	{{id, client, client_port}, rest} = Map.pop(queries, id)
	response = Msg.fake(msg, id)
	:gen_udp.send(local, client, client_port, response)
	dns(local, remote, server, server_port, n, rest)	

      :update ->
	DNS.dns(local, remote, server, server_port, n, queries)

      :status ->
	:io.format("local: ~w~nremote: ~w~nserver ~w~nport ~w~n", [local, remote, server, server_port])
	dns(local, remote, server, server_port, n, queries)

      :quit ->
	:ok

      strange ->
	:io.format("strange: ~w~n", [strange])
	dns(local, remote, server, server_port, n, queries)
    end
  end    

  
end
