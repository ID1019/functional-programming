defmodule Graph do

  def sample() do
    new(
      [a: [b: 4, c: 2],
       b: [c: 1, d: 2, e: 4],
       c: [f: 1, e: 3, h: 5],
       d: [g: 2],
       e: [f: 2],
       f: [h: 1],
       g: [h: 2]
      ])
  end

  def new(nodes) do
    Map.new(nodes)
  end

  def next(node, graph) do
    Map.get(graph, node, []) 
  end
end
