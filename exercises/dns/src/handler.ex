defmodule Handler do

  @port 53
  @timeout 5000

  # def handle(packet, ip, port, _socket, _server) do
  #   :io.format("Received request from ~w:~w  ~w~n", [ip, port, packet])
  # end

  def start(packet, repl, frw) do
    spawn(fn -> handler(packet, repl, frw) end)
  end

  def handler(packet, reply, frw) do
    :io.format("Handler received request: ~w~n", [packet])

    # trace(packet)
    case frw.(packet) do
      {:ok, answer} ->
        :io.format("Received reply: ~w~n", [answer])
        # trace(answer)
        reply.(answer)

      {:error, error} ->
        :io.format("Error in forward: ~w~n", [error])
    end
  end

end
