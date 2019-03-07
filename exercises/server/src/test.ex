defmodule Test do

  @number_requests 100  ## per client
  @number_clients 4

  def parse(n) do
    req =  'GET /foo HTTP/1.1\r\n\r\n'
    parse(n, req)
  end
  
  def parse(0,_) do :ok end
  def parse(n, req) do
    HTTP.parse_request(req)
    parse(n-1, req)
  end
    

  
  def bench(host, port) do
    bench(host, port, @number_requests * @number_clients)
  end
  
  def bench(host, port, requests) do
    t0 = :erlang.monotonic_time(:millisecond)
    case  run(requests, host, port) do
      :ok ->
	t1 = :erlang.monotonic_time(:millisecond)
	IO.puts("Benchmark: #{requests} requests in #{t1-t0} ms")
      :error ->
	IO.puts("Benchmark: failed")
    end
  end

  defp run(0, _host, _port), do: :ok
  defp run(n, host, port) do
    case request(host, port) do
      :ok ->
	run(n - 1, host, port)
      :error ->
	:error
    end
  end

  defp request(host, port) do
    opt = [:list, active: false, reuseaddr: true]
    case :gen_tcp.connect(host, port, opt) do
      {:ok, server} ->
	:gen_tcp.send(server, HTTP.get("/foo.html"))
	case :gen_tcp.recv(server, 0) do
	  {:ok, _reply} ->
	    :ok
	  {:error, _reason} ->
	    :error
	end
	:gen_tcp.close(server)
      {:error, _reason} ->
	:error
    end
  end

  def parallel(host, port) do
    parallel(host, port, @number_requests, @number_clients)
  end

  def parallel(host, port, requests, clients) do
    t0 = :erlang.monotonic_time(:millisecond)
    me = self()
    par(clients, host, port, requests, me)
    all = barrier(clients, [])
    t1 = :erlang.monotonic_time(:millisecond)
    IO.puts("Benchmark: #{clients} clients, #{requests} requests each, in #{t1-t0} ms")
    all
end    
  
  def par(0, _host, _port, _req, _me) do
    :ok
  end
  def par(n, host, port, requests, me) do
    spawn(fn() ->
      {t, _} = :timer.tc(fn() -> run(requests, host, port) end)
      send(me, {:done, div(t,1000)})
      end)
    par(n-1, host, port, requests, me)
  end


  def barrier(0, acc) do
    acc
  end
  def barrier(n, acc) do
    receive do
      {:done, t} ->
	barrier(n-1, [t|acc])
    end
  end  
end
