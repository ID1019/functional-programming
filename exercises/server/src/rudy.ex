defmodule Rudy do

  def start(port) do
    Process.register(spawn(fn -> init(port) end), :rudy)
  end

  def stop() do
    case Process.whereis(:rudy) do
      nil -> :ok
      pid ->
	Process.exit(pid, "Time to die!")
    end
  end

  # Initialize the server, takes a port number, open a listening socket 
  # and passe the socket to the handler/1. Once the request has been 
  # handled the socket is closed.
  defp init(port) do
    opt = [:list, active: false, reuseaddr: true]

    case :gen_tcp.listen(port, opt) do
      {:ok, listen} ->
        handler(listen)
        :gen_tcp.close(listen)
        :ok

      {:error, error} ->
        error
    end
  end

  # Listen to the socket for an incoming connection. Once a client has 
  # connected it passes the connection to request/1. When the request 
  # is handled the connection is closed.
  defp handler(listen) do
    :io.format("ready: ~n", [])
    case :gen_tcp.accept(listen) do
      {:ok, client} ->
	{:ok, {ip, port}} = :inet.peername(client)
	:io.format("new connection: ~w ~w ~n", [ip, port])
	request(client)
	:io.format("closing connection~n", [])
        :gen_tcp.close(client)
        handler(listen)

      {:error, error} ->
        error
    end
  end

  # Read the request from the client connection and parse it. It then 
  # parses the request using the http parser and pass the request to 
  # reply/1. The reply is then sent back to the client.
  defp request(client) do
    recv = :gen_tcp.recv(client, 0)
    case recv do
      {:ok, str} ->
	:io.format("request: ~s ~n", [str])
        request = HTTP.parse_request(str) 
	:io.format("parsed: ~p ~n", [request])
        response = Rudy.reply(request)
	:io.format("response: ~s~n", [response])
        ##response = dummy()	
        :gen_tcp.send(client, response)

      {:error, error} ->
        IO.puts("Rudy error: #{error}")
    end
  end

  # Decide what to reply and how to turn the reply into a well formed 
  # HTTP reply.
  def reply({{:get, uri, _}, _, _}) do
    Web.reply(uri)
  end

  defp dummy() do
    ## IO.puts("request #{uri}")
    :timer.sleep(10)
    HTTP.ok("Hello!")
  end

  
end
