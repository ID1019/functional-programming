defmodule Phil do

  @dreaming 8
  @eating 2
  @delay 1

  def start(name, left, right, hunger, ctrl) do
    spawn_link(fn() -> dreaming(name, left, right, hunger, ctrl) end)
  end

  def dreaming(name, _left, _right, 0, ctrl) do
    IO.puts(" #{name} is happy")
    send(ctrl, :done)
    :ok
  end
  def dreaming(name, left, right, hunger, ctrl) do
    ##IO.puts(" #{name} is dreaming")
    sleep(@dreaming)
    waiting(name, left, right, hunger, ctrl)
  end

  def waiting(name, left, right, hunger, ctrl) do
    IO.puts(" #{name} is waiting")
    Chop.request(left)
    sleep(@delay)
    Chop.request(right)
    eating(name, left, right, hunger, ctrl)
  end

  def eating(name, left, right, hunger, ctrl) do
    IO.puts(" #{name} is eating")
    sleep(@eating)
    Chop.return(left)
    Chop.return(right)
    dreaming(name, left, right, hunger-1, ctrl)
  end


  def sleep(0) do :ok end
  def sleep(t) do :timer.sleep(:rand.uniform(t)) end
  

end
