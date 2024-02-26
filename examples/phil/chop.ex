defmodule Chop do

  def start() do
    spawn_link(fn() -> available() end)
  end

  def request(chop) do
    send(chop, {:request, self()})
    receive do
      :granted ->
	:ok
    end
  end

  def return(chop) do
    send(chop, :return)
  end
  
  def available() do
    receive do
      {:request, from} ->
	send(from, :granted)
	gone()
      :quit ->
	:ok
    end
  end

  def gone() do
    receive do
      :return ->
	available()
      :quit ->
	:ok
    end
  end
  

end
