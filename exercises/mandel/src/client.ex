defmodule Client do

  ### Example of usage Client.start(:"gold@130.120.33.23", :server)
  ### Note how the node is writen
  
  def start(node, name) do
    case Node.connect(node) do
      true ->
	pid = :global.whereis_name(name)
	spawn(fn -> init(pid) end)
      _ ->
	:error
    end
  end

  defp init(server), do: client(server, 0)

  defp client(server, n) do
    send(server, {:request, self()})

    receive do
      {:task, w, h, {:trans, x, y, k}, depth, ctrl} ->
        IO.puts("#{w} #{h}")

        tr = fn(a, b) ->
          Cmplx.new(x + k * (a - 1), y - k * (b - 1))
        end

        row = row(w, h, tr, depth, [])
        send(ctrl, {:row, h, row})
        client(server, n + 1)

      :done ->
        IO.puts("Client completed #{n} rounds")
    end
  end

  defp row(0, _, _, _, row), do: row
  defp row(w, h, tr, depth, row) do
    c = tr.(w, h)
    res = Brot.mandelbrot(c, depth)
    color = Color.convert(res, depth)
    row(w - 1, h, tr, depth, [color | row])
  end

end
