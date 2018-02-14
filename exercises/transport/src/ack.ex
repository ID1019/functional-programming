defmodule Ack do

  defstruct seq: 0

  defimpl String.Chars, for: Ack do
    def to_string(%Ack{seq: n})  do
      "Ack<seq: #{n}>"
    end
  end


end

  
