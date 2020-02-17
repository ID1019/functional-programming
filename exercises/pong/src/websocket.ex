defmodule WebSocket do 

  def start(port, sessions) do
    spawn_link(fn -> init(port, sessions) end)
  end

  # Initialize the server: takes a port number and a set of session
  # processes. 

  defp init(port, sessions) do
    opt = [:binary, active: true, reuseaddr: true]
    case :gen_tcp.listen(port, opt) do
      {:ok, listen} ->
	handlers = Enum.map(sessions, fn(session) -> spawn_link(fn() -> connect(listen, session) end) end)
	receive do
	  :stop ->
	    # the socket process must live as long as the sessions last
	    Enum.each(handlers, fn(handler) -> send(handler, :stop) end)
	    :io.format("ws: server stopped ~n")
	    :ok
	end
      {:error, error} ->
        error
    end
  end

  
  ## Listen to the socket for an incoming connection. 

  def connect(listen, session) do
    :io.format("ws: session ready ~n")
    case :gen_tcp.accept(listen) do
      {:ok, socket} ->
	{:ok, {ip, port}} = :inet.peername(socket)
	:io.format("ws: new connection from ~w ~w ~n", [ip, port])
	case handshake(socket) do
	  :ok ->
	    send(session, {:ws,  self(), :open})
	    session(socket, session)
	    :gen_tcp.close(socket)
	  {:error, reason} ->
	    :io.format("ws: session failed ~w~n", [reason])
	end
      {:error, error} ->     
        error
    end
  end

  ## This is the handler for one session.

  defp session(socket, session) do
    receive do
      {:tcp, ^socket, msg} ->
	##:io.format("ws: session received messag~n")
	case decode(msg) do
	  {:ping, msg} ->
	    :gen_tcp.send(socket, pong(msg))
	    session(socket, session)	    
	  {:pong, _msg} ->
	    session(socket, session)	    
	  :closed ->
	    :io.format("ws: session closed by client~n")
	    send(session, {:ws, self(), :closed})
	  {:ok, msg} ->
	    ##:io.format("ws session frw messag: ~w~n", [msg])
	    send(session, {:ws, self(), {:msg, msg}})
	    session(socket, session)
	end
      {:tcp_closed, ^socket} ->
	:io.format("ws: socket closed by client~n")
	send(session, {:ws, self(), :closed})

      {:frw, msg} ->
	:gen_tcp.send(socket,  message(msg))
	session(socket, session)
      :stop ->
	:io.format("ws: session stopped~n")
	:ok
      error ->
	:io.format("ws: session error: ~w ~n", [error])
	:ok	
    end
  end

  ## The websocket specific handshake and coding of messages.
  
  defp handshake(socket) do
    receive do
      {:tcp, ^socket, request} ->
	{request, headers, <<>>} = parse_request(request)
	{:get, _uri, _ver} = request
	##:io.format("\n\n ~p\n\n", [headers])
        {"Upgrade", "websocket"} = List.keyfind(headers, "Upgrade", 0)
	{"Sec-WebSocket-Protocol", "pong"} = List.keyfind(headers, "Sec-WebSocket-Protocol", 0)
	{"Sec-WebSocket-Key", key} = List.keyfind(headers, "Sec-WebSocket-Key", 0)
	respons = respons(key)
	##:io.format("\n\n~s\n", [respons])
	:gen_tcp.send(socket, respons)
      {:tcp_closed, ^socket} ->
	{:error, :closed}
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
  ## contained in one frame.


  def decode(<<_frag::4, op::4, _m::1, _len::7, _rest::binary >>) when op == 8 do
    :closed
  end
  def decode(<<frag::4, op::4, m::1, len::7, rest::binary >>) when op == 9 do
    8 = frag   # first and only frame, no extensions
    1 = m      # masking turned on
    cond do
      len < 126 ->
	<<mask::binary-size(4), payload::binary-size(len), _::binary>> = rest
	{:ping, mask(payload, mask)}
      len == 126 ->
	<<len::16, mask::binary-size(4), payload::binary-size(len), _::binary>> = rest
	{:ping, mask(payload, mask)}
      true -> 
	<<len::64, mask::binary-size(4), payload::binary-size(len), _::binary>> = rest
	{:ping, mask(payload, mask)}
    end
  end  
  def decode(<<frag::4, op::4, m::1, len::7, rest::binary >>) when op == 10 do
    8 = frag   # first and only frame, no extensions
    1 = m      # masking turned on
    cond do
      len < 126 ->
	<<mask::binary-size(4), payload::binary-size(len), _::binary>> = rest
	{:pong, mask(payload, mask)}
      len == 126 ->
	<<len::16, mask::binary-size(4), payload::binary-size(len), _::binary>> = rest
	{:pong, mask(payload, mask)}
      true -> 
	<<len::64, mask::binary-size(4), payload::binary-size(len), _::binary>> = rest
	{:pong, mask(payload, mask)}
    end
  end  

  def decode(<<frag::4, op::4, m::1, len::7, rest::binary >>) when  (op ==1) or  (op == 2) do
    8 = frag   # first and only frame, no extensions
    1 = m      # masking turned on
    cond do
      len < 126 ->
	<<mask::binary-size(4), payload::binary-size(len), _::binary>> = rest
	{:ok, mask(payload, mask)}
      len == 126 ->
	<<len::16, mask::binary-size(4), payload::binary-size(len), _::binary>> = rest
	{:ok, mask(payload, mask)}
      true -> 
	<<len::64, mask::binary-size(4), payload::binary-size(len), _::binary>> = rest
	{:ok, mask(payload, mask)}
    end
  end

  def message(data) do
    op = 2 # binary
    encode(op, data)
  end
  def pong(data) do
    op = 10 # pong
    encode(op, data)
  end  

  def encode(op, data) do
    len = byte_size(data)
    mask = <<1,2,3,4>>   # this should of course be random
    xored = mask(data, mask)
    frag = 8      # single frame, no extensions
    op = op       # opcode
    flags = <<frag::4, op::4>>

    mask = cond do
      len < 126 ->
	<<1::1, len::7, mask::binary-size(4)>>
      len <= 65536 ->
	<<1::1, 126::7, len::16, mask::binary-size(4)>>
      true ->
	<<1::1, 127::7, len::64, mask::binary-size(4)>>
    end
    flags <> mask <> xored
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
    [head, body] = :binary.split(request, "\r\n\r\n")
    [request | headers] = :binary.split(head, "\r\n", [:global])
    request = request_line(request)
    headers = headers(headers)
    {request, headers, body}
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
