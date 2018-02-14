defmodule Ord do

  defstruct seq: 0,  data: nil

  defimpl String.Chars, for: Ord do
    def to_string(%Ord{seq: n, data: msg})  do
      "Ord<seq: #{n}, data: " <> String.Chars.to_string(msg) <> ">"
    end
  end


end

  
