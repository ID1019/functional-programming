defmodule WebSocket do 

  def start(port, sessions) do
    spawn_link(fn -> init(port, sessions) end)
  end

  # Initialize the server: takes a port number and a set of session
  # processes. As many handler processes as there are sessions will be
  # created. If one of them dies , all processes will die.

  defp init(port, sessions) do
    opt = [:binary, {:active, false}, {:reuseaddr, true}]
    case :gen_tcp.listen(port, opt) do
      {:ok, listen} ->
	Enum.map(sessions, fn(session) -> spawn_link(fn() -> connect(listen, session) end) end)
	receive do
	  :stop ->
	    # the socket process must live as long as the sessions last
	    :io.format("ws: server stopped ~n")
	    Process.exit(self(), :kill)
	end
      {:error, error} ->
       error
    end
  end

  
  ## Listen to the socket for an incoming connection. If handshake
  ## succeeds a decoder process is spawned to parse incoming
  ## messages. The messages are send to the handler process that then
  ## sends them to the session process.

  def connect(listen, session) do
    :io.format("ws: session ready ~n")
    case :gen_tcp.accept(listen) do
      {:ok, socket} ->
	{:ok, {ip, port}} = :inet.peername(socket)
	:io.format("ws: new connection from ~w ~w ~w ~n", [ip, port, socket])
	case handshake(socket, <<>>) do
	  :ok ->
	    :io.format("ws: handshake completed ~n")	    
	    me = self()
	    spawn_link(fn() -> decoder(socket, me, <<>>) end)
	    send(session, {:ws, self(), :open})
	    handler(socket,  session)
	    :gen_tcp.close(socket)
	  {:error, reason} ->
	    :io.format("ws: handshake failed ~w~n", [reason])
	end
      {:error, error} ->     
        error
    end
    Process.exit(self(), :kill)
  end

  def handler(socket, session) do
    receive do
      {:msg, msg} ->
	send(session,  {:ws, self(), {:msg, msg}})
	handler(socket, session)
      :closed ->
	send(session,  {:ws, self(), :closed})	

      {:frw, msg} ->
	:gen_tcp.send(socket,  message(msg))
	handler(socket, session)

      :stop ->
	:gen_tcp.send(socket,  close())

      strange ->
	:io.format("ws: strange message: ~w ~n", [strange])

    end
  end

  ## The websocket specific handshake and coding of messages.
  
  defp handshake(socket, sofar) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, more} ->
	sofar = more <> sofar

	case parse_request(sofar) do
	  {request, headers} -> 
	    {:get, _uri, _ver} = request
	    ##:io.format("ws:\n\n ~p\n\n", [headers])
            {"Upgrade", "websocket"} = List.keyfind(headers, "Upgrade", 0)
	    {"Sec-WebSocket-Protocol", "pong"} = List.keyfind(headers, "Sec-WebSocket-Protocol", 0)
	    {"Sec-WebSocket-Key", key} = List.keyfind(headers, "Sec-WebSocket-Key", 0)
	    respons = respons(key)
	    ##:io.format("ws:\n\n~s\n", [respons])
	    :gen_tcp.send(socket, respons)
	    :ok
	  :more ->
	    # more bytes required
	    :io.format("ws: handshake, more bytes required\n")
	    handshake(socket, sofar)
	end
      {:error, reason} ->
	{:error, reason}
    end
  end
  
  
  ## This is the decoder of a session handler. All decoded messages
  ## will be sen to the handler.

  defp decoder(socket, handler, <<>>) do
    case :gen_tcp.recv(socket,0) do
      {:ok, more} ->
	decoder(socket, handler, more)
      {:error, reason} ->
	:io.format("ws: socket closed by client (~w)~n", [reason])
	Process.exit(self(), :kill)	
    end
  end
  
  defp decoder(socket, handler, sofar) do
    case decode(sofar) do
      {:ping, msg, rest} ->
	:gen_tcp.send(socket, pong(msg))
	decoder(socket, handler, rest)	    
      {:pong, _msg, rest} ->
	decoder(socket, handler, rest)
      :closed ->
	:io.format("ws: websocket closed by client\n")
	send(handler, :closed)
	Process.exit(self(), :kill)	
      {:ok, msg, rest} ->
	send(handler,  {:msg, msg})
	decoder(socket, handler, rest)
    end
  end


     #  0                   1                   2                   3
     #  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     # +-+-+-+-+-------+-+-------------+-------------------------------+
     # |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
     # |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
     # |N|V|V|V|       |S|             |   (if payload len==126/127)   |
     # | |1|2|3|       |K|             |                               |
     # +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
     # |     Extended payload length continued, if payload len == 127  |
     # + - - - - - - - - - - - - - - - +-------------------------------+
     # |                               |Masking-key, if MASK set to 1  |
     # +-------------------------------+-------------------------------+
     # | Masking-key (continued)       |          Payload Data         |
     # +-------------------------------- - - - - - - - - - - - - - - - +
     # :                     Payload Data continued ...                :
     # + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
     # |                     Payload Data continued ...                |
     # +---------------------------------------------------------------+

  ## We will only handle non-fragmented messages that are fully
  ## contained in one frame i.e. frag == 8

  ## One binary can however contain several full frames.

  def decode(<<_frag::4, op::4, _m::1, _len::7, _rest::binary >>) when op == 8 do
    :closed
  end
  def decode(<<8::4, op::4, m::1, len::7, rest::binary >>) when op == 9 do
    {data, rest} = demask(m, len, rest)
    {:ping, data, rest}
  end  
  def decode(<<8::4, op::4, m::1, len::7, rest::binary >>) when op == 10 do
    {data, rest} = demask(m, len, rest)    
    {:pong, data, rest}
  end  
  def decode(<<8::4, op::4, m::1, len::7, rest::binary >>) when  (op ==1) or  (op == 2) do
    {data, rest} = demask(m, len, rest)    
    {:ok, data, rest}
  end

  def demask(1, len, rest) when len < 126 do
    <<mask::binary-size(4), payload::binary-size(len), rest::binary>> = rest
    {mask(payload, mask), rest}
  end
  
  def demask(1, 127, rest) do
    <<len::16, mask::binary-size(4), rest::binary>> = rest
    <<payload::binary-size(len), rest::binary>> = rest
    {mask(payload, mask), rest}
  end
  def demask(1, 128, rest) do 
    <<len::64, mask::binary-size(4), rest::binary>> = rest
    <<payload::binary-size(len), rest::binary>> = rest
    {mask(payload, mask), rest}
  end

  

  def message(data) do
    op = 2 # binary
    encode(op, data)
  end

  def pong(data) do
    op = 10 # pong
    encode(op, data)
  end

  def close() do
    op = 8 # close
    encode(op, <<>>)
  end


  def encode(op, data) do
    len = byte_size(data)
    frag = 8      # single frame, no extensions
    op = op       # opcode
    flags = <<frag::4, op::4>>
    # data is not masked by the server
    nomask = cond do
       len < 126 ->
     	<<0::1, len::7>>
       len <= 65536 ->
     	<<0::1, 126::7, len::16>>
       true ->
     	<<0::1, 127::7, len::64>>
     end
    flags <> nomask <> data
  end

  

  def mask(<<word::binary-size(4), rest::binary>>, mask) do
    :crypto.exor(word, mask) <> mask(rest, mask)
  end
  def mask(<<byte::binary-size(1), rest::binary>>, <<msk::binary-size(1), mask::binary>>) do
    :crypto.exor(byte, msk) <> mask(rest, mask)
  end
  def mask(<<>>, _) do <<>> end
    

  ## This is HTTP decoding and encoding 

  #GET /chat HTTP/1.1
  #Host: server.example.com
  #Upgrade: websocket
  #Connection: Upgrade
  #Sec-WebSocket-Key: x3JJHMbDL1EzLkh9GBhXDw==
  #Sec-WebSocket-Protocol: chat, superchat
  #Sec-WebSocket-Version: 13
  #Origin: http://example.com	

  #HTTP/1.1 101 Switching Protocols
  #Upgrade: websocket
  #Connection: Upgrade
  #Sec-WebSocket-Accept: HSmrc0sMlYUkAGmm5OPpG2HaGWk=
  #Sec-WebSocket-Protocol: chat

  def respons(key) do
    resp = :base64.encode(:crypto.hash(:sha, key <> "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))
    "HTTP/1.1 101 Switching Protocols\r\n" <>
      "Upgrade: websocket\r\n" <>
      "Connection: Upgrade\r\n" <>
      "Sec-WebSocket-Version: 13\r\n" <>
      "Sec-WebSocket-Protocol: pong\r\n" <>
      "Sec-WebSocket-Accept: " <>  resp <> "\r\n" <>
      "\r\n"
  end
  	
	
  defp parse_request(request) do
    case :binary.part(request, byte_size(request), -4) do
      "\r\n\r\n" ->
	request = :binary.part(request, 0, byte_size(request) - 4)
	[request | headers] = :binary.split(request, "\r\n", [:global])
	request = request_line(request)
	headers = headers(headers)
	{request, headers}
      _ ->
	:more
    end
  end
  
  defp request_line(line) do
    [<<?G, ?E, ?T>>, uri, <<?H, ?T, ?T, ?P, ?/, ?1, ?., ?1>>] = :binary.split(line, <<32>>, [:global])
    {:get, uri, :v11}
  end

  defp headers(rows) do
    Enum.map(rows, fn(row) ->
      [key, value] = :binary.split(row, <<?:,32>>)
      {key, value}
    end)
  end


end
