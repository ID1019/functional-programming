defmodule Node do

defstruct key: nil, value: nil, left: nil, right: nil

def update(nil, key, value) do
  %Node{key: key, value: value, left: nil, right: nil}
end

def update(%Node{} = node, key, value) do
  cond do
    key == node.key ->
      %Node{
        key: key,
        value: value,
        left: node.left,
        right: node.right
      }

    key < node.key ->
      {:splay, _, a, b} = splay(node.left, key)
      %Node{
        key: key,
        value: value,
        left: a,
        right: {:node, node.key, node.value, b, node.right}
      }

    true ->
      {:splay, _, b, c} = splay(node.right, key)
      %Node{
        key: key,
        value: value,
        left: {:node, node.key, node.value, node.left, b},
        right: c
      }
  end
end

end
