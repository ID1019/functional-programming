defmodule Handler do

  @port 53
  @timeout 5000

  def handle(packet, ip, port, _socket, _server) do
    IO.puts("Received request from #{ip}:#{port}:#{packet}")
  end    
end