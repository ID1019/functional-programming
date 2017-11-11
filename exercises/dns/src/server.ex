defmodule Server do

  @server "8.8.8.8" # The Google DNS server
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
            IO.puts("DNS error opening server socket: #{error}")
        end
      error ->
        IO.puts("DNS error opening server socket: #{error}")
    end
  end

  defp dns(local, remote, server, n, msgs) do
    receive do
      {:udp, ^local, ip, port, <<id :: size(16), request :: binary>>} ->
        IO.puts("Request #{ip}:#{port} #{id}")
        :gen_udp.send(remote, server, @remote_port, <<n :: size(16), request :: binary>>)
        dns(local, remote, server, n + 1, [{n, ip, port, id} | msgs])
      {:udp, ^remote, _ip, _port, <<r :: size(16), reply :: binary>>} ->
        IO.puts("Reply message: #{r}")
        case List.keyfind(msgs, r, 0) do
          {^r, ip, port, id} ->
            :gen_udp.send(local, ip, port, <<id :: size(16), reply :: binary>>)
            dns(local, remote, server, n, List.keydelete(msgs, r, 0))
          false ->
            dns(local, remote, server, n, msgs)
        end
      :update -> 
        dns(local, remote, server, n, msgs)
      :stop ->
        IO.puts("Bye, bye!")
        :ok
      error ->
        IO.puts("Strange message: #{error}")
        dns(local, remote, server, n, msgs)
    end
  end

end