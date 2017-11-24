defmodule Chopstick do

  def start do
    stick = spawn_link(fn -> init() end)
    {:stick, stick}
  end


  ## The synchronous version of requesting a chopstick.
  def request({:stick, pid}) do
    send pid, {:request, self()}
    receive do
      :granted -> :ok
    end
  end

  def return({:stick, pid}) do
    send pid, :return
  end


  ## Using a timeout to detect deeadlock, does it work?
  def request({:stick, pid}, timeout) do
    send pid, {:request, self()}
    receive do
      :granted -> 
        :ok
    after timeout -> 
      :no
    end
  end


  ## The better version, we keep track of requests.
  def request({:stick, pid}, ref, timeout) do
    send pid, {:request, ref, self()}
    wait(ref, timeout)
  end

  def wait(ref, timeout) do
    receive do
      {:granted, ^ref} ->
        :ok
      {:granted, _} ->
        wait(ref, timeout)
    after timeout ->
      :no
    end
  end

  def return({:stick, pid}, ref) do
    send pid, {:return, ref}
  end


  ## A asynchronous request, divided into sending the request and
  ## waiting for the reply.
  def asynch({:stick, pid}, ref) do
    send pid, {:request, ref, self()}
  end


  ##To terminate the process.
  def quit({:stick, pid}) do
    send pid, :quit
  end


  ##Initalizing the chopstick.
  def init(), do: available()

  ## The two states of the chopstick.
  def available() do
    receive do
      {:request, from} ->
        send from, :granted
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