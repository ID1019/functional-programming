defmodule Prgm do

  def lookup(id, prg) do
    List.keyfind(prg, id, 0)
  end

end


		       
