defmodule Msg do

  defstruct data: nil

  defimpl String.Chars, for: Dgr do
    def to_string(%Msg{data: data})  do
      "<Msg: data: " <> String.Chars.to_string(data) <> ">"
    end
  end


end
