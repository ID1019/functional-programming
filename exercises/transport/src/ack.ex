defmodule Ack do

  defstruct id: 0

  defimpl String.Chars, for: Ack do
    def to_string(%Ack{id: n})  do
      "<Ack: #{n} >"
    end
  end


end

  
