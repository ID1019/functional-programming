defmodule Handler do

  @port 53
  @timeout 5000

  # def handle(packet, ip, port, _socket, _server) do
  #   IO.puts("Received request from #{ip}:#{port}:#{packet}")
  # end

  def start(packet, repl, frw) do
    spawn(fn -> handler(packet, repl, frw) end)
  end

  def handler(packet, reply, frw) do
    IO.puts("Handler received request: #{packet}")

    # trace(packet)
    case frw.(packet) do
      {:ok, answer} ->
	      IO.puts("Received reply: #{answer}")
	      # trace(answer)
	      reply.(answer)
	    {:error, error} ->
	      IO.puts("Error in forward: #{error}")
    end
  end
end