defmodule Trudy do

  @handlers 8
  @name :trudy

  def start(port) do
    Process.register(spawn(fn -> init(port) end), @name)
  end

  def stop() do
    case Process.whereis(@name) do
      nil ->
	:ok
      pid ->
	## We need to send a "kill" signal since the server
	## traps all exit signals.
	Process.exit(pid, :kill)
    end
  end

  # Initialize the server, takes a port number, open a listening socket 
  # and passe the socket to the handler/1. Once the request has been 
  # handled the socket is closed.
  defp init(port) do
    opt = [:list, active: false, reuseaddr: true]

    case :gen_tcp.listen(port, opt) do
      {:ok, listen} ->
	## This is to detect failing handlers
	Process.flag(:trap_exit, true)
        handlers(@handlers, listen)
        :gen_tcp.close(listen)
        :ok
      {:error, error} ->
        error
    end
  end

  defp supervise(listen) do
    receive do
      {:EXIT, _pid, reason}  ->
	## A handler (or actually any exit signal) has crached"
	log(reason)
	spawn_link(fn() -> handler(listen) end)
	supervise(listen)
      strange ->
	:io.format("strange message: ~w~n", [strange])	
	supervise(listen)
    end
  end

  defp log(reason) do
    if is_binary(reason) do
      :io.format("handler died: ~s~n", [reason])
    else
      :io.format("handler died: ~w~n", [reason])
    end
  end
  

  defp handlers(0, listen) do
    supervise(listen)
  end
  defp handlers(n, listen) do
    spawn_link(fn() -> handler(listen) end)
    handlers(n-1,  listen)
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
        :gen_tcp.send(client, response)
      {:error, error} ->
        IO.puts("RUDY ERROR: #{error}")
    end
    :gen_tcp.close(client)
  end

  # Decide what to reply and how to turn the reply into a well formed 
  # HTTP reply.
  defp reply({{:get, _uri, _}, _, _}) do
    :timer.sleep(10)
    HTTP.ok("Hello!")
  end


  defp reply_file({{:get, uri, _}, _, _}) do
    Web.reply(uri)
  end

end
