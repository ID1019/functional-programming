defmodule Relay do

  @server "8.8.8.8" # The Google DNS server
  @port 53

  def start, do: start(@server)
  def start(server), do: spawn_link(fn -> init(server) end)

  defp init(server) do
    case :gen_udp.open(0, [{:active, true}, :binary]) do
      {:ok, socket} ->
        relay(1, socket, server, [])
      error ->
        {:error, error}
    end
  end

  defp relay(n, socket, server, msgs) do
    receive do
      {:forward, from, <<id :: size(16), request :: binary>>} ->
        IO.puts("Forward message: #{id}/#{n}")
        :gen_udp.send(socket, server, @port, <<n :: size(16), request :: binary>>)
        relay(n + 1, socket, server, [{from, n, id} | msgs])
      {:udp, socket, _ip, _port, <<r :: size(16), reply :: binary>>} ->
        IO.puts("Reply message: #{r}")
        case List.keyfind(msgs, r, 1) do
          {from, ^r, id} ->
            send from, {:reply, <<id :: size(16), reply :: binary>>}
            relay(n, socket, server, List.keydelete(msgs, r, 1))
          false ->
            relay(n, socket, server, msgs)
        end
      :update ->
        relay(n, socket, server, msgs)
      :stop ->
        :ok
      strange ->
        IO.puts("Strange message: #{strange}")
        relay(n, socket, server, msgs)
    end
  end
end