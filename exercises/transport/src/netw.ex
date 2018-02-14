defmodule Netw do

 defstruct src: 0, dst: 0, data: nil

  defimpl String.Chars, for: Netw do
    def to_string(%Netw{src: s, dst: d, data: msg})  do
      "Netw<src: #{s}, dst: #{d}, data: " <> String.Chars.to_string(msg) <> ">"
    end
  end


end

  
