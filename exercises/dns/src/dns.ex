defmodule DNS do

  @server "8.8.8.8"
  @port 53
  @local 5300
  @timeout 4000

  def start, do: start(@local, @server)
  def start(port), do: start(port, @server)
  def start(port, server) do
    spawn_link(fn -> init(port, server) end)
  end

  def init(port, server) do
    case :gen_udp.open(port, [{:active, true}, :binary]) do
      {:ok, socket} ->
        dns(socket, server)

      error ->
        IO.puts("DNS error opening server socket: #{error}")
    end
  end

  def dns(socket, server) do
    receive do
      {:udp, socket, ip, port, packet} ->
        reply = fn(rep) -> :gen_udp.send(socket, ip, port, rep) end
        frw = fn(req) -> forward(req, server) end
        Handler.start(packet, reply, frw)
        dns(socket, server)

      {:update} ->
        dns(socket, server)

      {:stop} ->
        IO.puts("Bye, bye!")
        :ok

      error ->
        IO.puts("Strange message: #{error}")
        dns(socket, server)
    end
  end

  def forward(request, server) do
    case :gen_udp.open(0, [{:active, true}, :binary]) do
      {:ok, client} ->
        :gen_udp.send(client, server, @port, request)

        result =
          receive do
            {:udp, ^client, _ip, _port, reply} ->
              {:ok, reply}
          after
            @timeout ->
              {:error, :timeout}
          end

        :gen_udp.close(client)
        result

      error ->
        {:error, error}
    end
  end

end
