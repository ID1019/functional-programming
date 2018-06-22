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
    case :gen_tcp.accept(listen) do
      {:ok, client} ->
        request(client)
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
        request = HTTP.parse_request(str) 
        response = reply(request)
        ##response = dummy()	
        :gen_tcp.send(client, response)

      {:error, error} ->
        IO.puts("RUDY ERROR: #{error}")
    end

    :gen_tcp.close(client)
  end

  # Decide what to reply and how to turn the reply into a well formed 
  # HTTP reply.
  defp reply({{:get, _uri, _}, _, _}) do
    ## IO.puts("request #{uri}")
    :timer.sleep(10)
    HTTP.ok("Hello!")
  end

  defp dummy() do
    ## IO.puts("request #{uri}")
    :timer.sleep(10)
    HTTP.ok("Hello!")
  end

end
