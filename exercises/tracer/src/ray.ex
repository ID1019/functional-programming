defmodule Ray do
  
  require Record

  Record.defrecord(:ray, pos: {0,0,0}, dir: {0,0,1})

end
