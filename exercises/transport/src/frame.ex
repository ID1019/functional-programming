defmodule Frame do

  defstruct data: nil

  defimpl String.Chars, for: Frame do
    def to_string(%Frame{data: msg})  do
      "Frame<data: " <> String.Chars.to_string(msg) <> ">"
    end
  end


end

  
