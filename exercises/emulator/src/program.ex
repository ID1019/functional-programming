defmodule Program do

  def load({:prgm, code, data}) do
    {data, labels} = endata(data) 
    code = encode(code, labels)
    {{:code, List.to_tuple(code)}, {:data, Map.new(data)}}
  end

  def read_instruction({:code, code}, pc) do
    0 = rem(pc,4) 
    elem(code, div(pc,4))
  end


  def endata(data) do  endata(data, 0, [], []) end

  def endata([], _, words, labels) do  {words, labels}   end
  def endata([spec|descr], n, words, labels) do
    {n, words, labels} = enspec(spec, n, words, labels)
    endata(descr, n, words, labels)
  end


  def enspec({:label, name}, n, words, labels) do
    {n, words, [{name, n}|labels]}
  end
  def enspec({:space, i}, n, words, labels) do
    {n, words} =  spaces(i, n, words)
    {n, words, labels}
  end
  def enspec({:word, values}, n, words, labels) when is_list(values) do
    {n, words} = List.foldl(values, {n, words},
      fn(val, {i, sofar}) ->
        enword(val, i, sofar)
      end)
    {n, words, labels}
  end

  def enspec({:word, value}, n, words, labels) when is_integer(value) do
    {n, words} = enword(value, n, words)
    {n, words, labels}
  end

  def enword(value, n, words) do
    {n+4, [{n,value}|words]}
  end

  def spaces(i, n, words) do
    ## adjust to multiple of 4 
    a = i + rem(4-rem(i,4), 4)
    {n+a, List.foldr(:lists.seq(n, n-1+a, 4), words, fn(x,acc) ->   [{x, 0}|acc] end)}
  end


  def read_word({:data, data}, i) do
    0 = rem(i,4)    ## addr must be amultiple of 4
    Map.get(data, i)
  end

  def write_word({:data, data}, i, val) do
    0 = rem(i, 4)   ## addr must be amultiple of 4
    {:data, Map.put(data, i, val)}
  end  



  def encode(prgm, labels) do
    {prgm, n, labels}  = collect(prgm, labels)
    ## The program is in reversed order, this i by intention, we can
    ## now do the encoding of the code with out a reverse operation.
    encode(prgm, n, [], labels)
  end


  

  ## Collecting and removeing labels from code segment.

  def collect(data, labels) do 
    collect(data, 0, [], labels)
  end
  
  def collect([], n, data, labels) do {data, n, labels} end
  def collect([{:label, name}|rest], n, data, labels) do
    collect(rest, n, data, [{name, n} | labels]) 
  end
  def collect([val|rest], n, data, labels) do
    collect(rest, n+4, [val|data],  labels)
  end

  ## Encoding instructions using the labels collected.
  
  def encode([], _, prgm, _) do prgm end
  def encode([instr| rest], n, prgm, labels) do
    encode(rest, n-4, [encode(instr, n, labels)|prgm], labels)
  end
  

  ## These are the instructions that we will use. More instructions
  ## are easily added to the system.
  
  def encode(instr, addr, labels) do
    case instr do

      {:addi, rt, rs, imm} when is_atom(imm) ->
	{:addi, rt, rs,  immediate(imm, labels)}

      {:ori, rt, rs, imm} when is_atom(imm) ->
	{:orii, rt, rs,  immediate(imm, labels)}

      {:lw, rt, rs, imm} when is_atom(imm) ->
	{:lw, rt, rs,  immediate(imm, labels)}

      {:sw, rt, rs, imm} when is_atom(imm) ->
	{:sw, rt, rs,  immediate(imm, labels)}


      {:beq,  rs, rt, offs} when is_atom(offs) ->
	{:beq, rs, rt, offset(offs, addr, labels)}
	
      {:bne,  rs, rt, offs} when is_atom(offs) ->
	{:bne, rs, rt, offset(offs, addr, labels)}

      instr -> instr
    end
  end
  
  ## If the immediate value is a label it is resolved.
  def immediate(label, labels) do
    {_, addr} = List.keyfind(labels, label, 0)
    addr
  end

  ## If the offset value is a label it is resolved.
  def offset(label, addr, labels) do
    {_, abs} = List.keyfind(labels, label, 0)
    (abs - addr)
  end
    

  
  
end
