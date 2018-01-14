defmodule Wait do

  def hello do
    receive do
      x -> IO.puts("aaa! surprise, a message: #{x}")
    end
  end
end
