defmodule Multi do

def union([], y) do y end
def union([h|t], y) do
   z = union(t,y)
   [h|z]
end

def tailr([], y) do y end
def tailr([h|t], y) do 
  z = [h|y]
  tailr(t,z)
end

end

