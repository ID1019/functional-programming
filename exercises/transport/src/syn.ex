defmodule Syn do

  defstruct add: 0

  defimpl String.Chars, for: Syn do
    def to_string(%Syn{add: n})  do
      "<Syn: add: #{n} >"
    end
  end


end
