defmodule Server do

  # The Google DNS server
  @server {8,8,8,8}
  @remote_port 53
  @local_port 5300
  @timeout 4000

  def start, do: start(@local_port, @server)
  def start(local_port), do: start(local_port, @server)
  def start(local_port, server) do
    spawn_link(fn -> init(local_port, server) end)
  end

  defp init(local_port, server) do
    case :gen_udp.open(local_port, [{:active, true}, :binary]) do
      {:ok, local} ->
        case :gen_udp.open(0, [{:active, true}, :binary]) do
          {:ok, remote} ->
            dns(local, remote, server, 1, [])

          error ->
            :io.format("DNS error opening server socket: ~w~n", [error])
        end

      error ->
        :io.format("DNS error opening server socket: ~w~n", [error])
    end
  end

  defp dns(local, remote, server, n, msgs) do
    receive do
      {:udp, ^local, ip, port, <<id::size(16), request::binary>>} ->
        :io.format("Request ~w:~w ~w~n", [ip, port, id])
        :gen_udp.send(remote, server, @remote_port, <<n::size(16), request::binary>>)
        dns(local, remote, server, n + 1, [{n, ip, port, id} | msgs])

      {:udp, ^remote, _ip, _port, <<r::size(16), reply::binary>>} ->
        :io.format("Reply message: ~w~n", [reply])

        case List.keyfind(msgs, r, 0) do
          {^r, ip, port, id} ->
            :gen_udp.send(local, ip, port, <<id::size(16), reply::binary>>)
            dns(local, remote, server, n, List.keydelete(msgs, r, 0))

          false ->
            dns(local, remote, server, n, msgs)
        end

      :update ->
        dns(local, remote, server, n, msgs)

      :stop ->
        :io.format("Bye, bye!~n")
        :ok

      error ->
        :io.format("Strange message: ~w~n", [error])
        dns(local, remote, server, n, msgs)
    end
  end

end
