defmodule Out do

  def new() do  [] end

  def put(out, a) do [a|out] end

  def close(out) do Enum.reverse(out) end

end
