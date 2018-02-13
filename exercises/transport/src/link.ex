defmodule Link do

  def start(master) do
    {:ok, spawn(fn() -> init(master) end)}
  end

  defp init(master) do
    receive do
      {:connect, lnk} ->
	:io.format("link ~w: connected to ~w~n", [self(), lnk])
	link(master, lnk)
      :quit ->
	:ok
    end
  end

  def link(master, lnk) do
    receive  do
      {:send, msg} ->
	send(lnk, %Frame{data: msg})
	link(master, lnk)

      %Frame{data: msg} ->
	send(master, msg)
	link(master, lnk)

      {:master, new} ->
	link(new, lnk)

      :status ->
	:io.format("link ~w: master: ~w,  link: ~w~n", [self(), master, lnk])
	link(master, lnk)

      :quit ->
	:ok
    end
  end

end
