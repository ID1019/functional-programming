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

  defp supervise(listen, n) do
    receive do
      {:EXIT, _pid, reason}  ->
	## A handler (or actually any exit signal) has crached"
	log(reason)
	spawn_link(fn() -> handler(listen, n) end)
	supervise(listen, n-1)
      strange ->
	:io.format("strange message: ~w~n", [strange])	
	supervise(listen, n)
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
    supervise(listen, 0)
  end
  defp handlers(n, listen) do
    spawn_link(fn() -> handler(listen, n) end)
    handlers(n-1,  listen)
  end  

  
  # Listen to the socket for an incoming connection. Once a client has 
  # connected it passes the connection to request/1. When the request 
  # is handled the connection is closed.
  defp handler(listen, n) do
    case :gen_tcp.accept(listen) do
      {:ok, client} ->
	{:ok, {ip, port}} = :inet.peername(client)
	:io.format("handler: ~w, ip: ~w\n", [n,ip])
        request(client, n, ip)
	:gen_tcp.close(client)
        handler(listen, n)

      {:error, error} ->
        error
    end
  end

  # Read the request from the client connection and parse it. It then 
  # parses the request using the http parser and pass the request to 
  # reply/1. The reply is then sent back to the client.
  defp request(client, n, ip) do
    recv = :gen_tcp.recv(client, 0)
    case recv do
      {:ok, str} ->
	:io.format("request: ~s\n", [str])
        request = HTTP.parse_request(str)
        response = echo(str, ip)
        :gen_tcp.send(client, response)
      {:error, :closed} ->
	:io.format("handler: ~w, closed\n", [n])
	:ok
      {:error, error} ->
	:io.format("handler: ~w, error ~w\n", [n, error])
	:ok
    end
    :gen_tcp.close(client)

  end

  # Decide what to reply and how to turn the reply into a well formed 
  # HTTP reply.
  defp reply({{:get, _uri, _}, _, _}) do
    ##:timer.sleep(10)
    HTTP.ok("Hello!")
  end

  defp echo(str, {a,b,c,d}) do
    reply = "request from: #{a}.#{b}.#{c}.#{d}\n" <> List.to_string(str)
    :io.format("reply: ~s\n", [reply])
    HTTP.ok(reply)
  end
  

  defp reply_file({{:get, uri, _}, _, _}) do
    Web.reply(uri)
  end

  
end
