defmodule Program do

  use Bitwise

  ## op codes
  @aop 0

  @beq 0x4
  @bne 0x5

  @addi 0x8
  @ori 0xd

  @lb 0x20
  @lw 0x23
  @sb 0x28
  @sw 0x2b

  @out  0x3e
  @halt 0x3f  
  
  ## fnct codes 
  @add 32
  @sub 34
  

  ## Give an assembler description of a program we create to data
  ## structure: one for the code segment and one for the data
  ## segment. The code segment is read only so we can implement it as
  ## a tuple indexed by instruction index (div(pc,4)). The memory is
  ## implemented as a map addressed by byte address.

  ## All instructions and values are encoded as 32-bit binaries. 
  
  def load({:prgm, code, data}) do
    {data, labels} = endata(data) 
    code = encode(code, labels)
    {{:code, List.to_tuple(code)}, {:data, Map.new(data)}}
  end


  ## The code segment is only read and we only allow program counters
  ## that are a multiple of four.
  
  def read_instruction({:code, code}, pc) do
    0 = rem(pc,4)      ## pc must be amultiple of 4
    elem(code, div(pc, 4))
  end


  ## The memory segment is address by byte but internally organized as
  ## words. When reading or writing bytes we need to do some byte
  ## fidling.
  
  
  def read_word({:data, data}, i) do
    0 = rem(i,4)    ## addr must be amultiple of 4
    <<val::32>> = Map.get(data, i)
    val
  end

  def write_word({:data, data}, i, val) do
    0 = rem(i, 4)   ## addr must be amultiple of 4
    val = <<val::32>>
    {:data, Map.put(data, i, val)}
  end  

  def read_byte({:data, data}, i) do
    j = rem(i,4)
    i = i - j
    <<a::8, b::8, c::8, d::8>> =  Map.get(data, i)
    ## This is a big-endian decision
    case j do
      0 -> a
      1 -> b
      2 -> c
      3 -> d
    end
  end

  def write_byte({:data, data}, i, byte) do
    j = rem(i,4)
    i = i - j
    <<a::8, b::8, c::8, d::8>> =  Map.get(data, i)
    ## This is a big-endian decision
    val = case j do
	    0 -> <<byte::8, b::8, c::8, d::8>>
	    1 -> <<a::8, byte::8, c::8, d::8>>
	    2 -> <<a::8, b::8, byte::8, d::8>>
	    3 -> <<a::8, b::8, c::8, byte::8>>
	  end
    {:data, Map.put(data, i, val)}
  end    
  

  ## Endata will encode the data segments and collect all labels. The
  ## labels will then be used in the encoding of the code segment.
  ##
  ## The encoded data is reapresented as a list of tuples {addr, word}
  ## where word is a 32 bit binary.  
  
  def endata(data) do endata(data, 0, [], []) end

  def endata([], _, bytes, labels) do
    {Enum.reverse(bytes), labels}
  end
  def endata([spec|descr], n, bytes, labels) do
    {n, bytes, labels} = enspec(spec, n, bytes, labels)
    endata(descr, n, bytes, labels)
  end


  def enspec({:label, name}, n, bytes, labels) do
    {n, bytes, [{name, n}|labels]}
  end
  def enspec({:space, i}, n, bytes, labels) do
    {n, bytes} =  spaces(i, n, bytes)
    {n, bytes, labels}
  end
  def enspec({:word, values}, n, bytes, labels) when is_list(values) do
    {n, bytes} = List.foldl(values, {n, bytes},
      fn(val, {i, sofar}) ->
        enword(val, i, sofar)
      end)
    {n, bytes, labels}
  end

  def enspec({:word, value}, n, bytes, labels) when is_integer(value) do
    {n, bytes} = enword(value, n, bytes)
    {n, bytes, labels}
  end

  def enspec({:byte, values}, n, bytes, labels) when is_list(values) do
    {n, bytes} = enbytes(values, n, bytes)
    {n, bytes, labels}
  end

  def enspec({:ascii, string}, n, bytes, labels) do
    {n, bytes}  =  enbytes(string, n, bytes)
    {n, bytes, labels}
  end
  def enspec({:asciiz, string}, n, bytes, labels) do
    {n, bytes}  =  enbytes(string ++ [0], n, bytes)
    {n, bytes, labels}
  end

  def enword(value, n, bytes) do
    ## the integer value is stored as a 32-bit binary, the deault is big-endian
    value = <<value::32>>
    {n+4, [{n,value}|bytes]}
  end


  def enbytes([], n, bytes) do {n, bytes} end  
  def enbytes([a,b,c,d|rest], n, bytes) do
    ## this is a big-endian litle-endian decission
    value = <<a::8,b::8,c::8,d::8>>
    enbytes(rest, n+4, [{n, value}|bytes]) 
  end  
  def enbytes([a,b,c], n, bytes) do 
    enbytes([a,b,c,0], n, bytes)
  end
  def enbytes([a,b], n, bytes)  do
    enbytes([a,b,0,0], n, bytes)
  end    
  def enbytes([a], n, bytes)  do
    enbytes([a,0,0,0], n, bytes)
  end

  def spaces(0, n, bytes) do {n, bytes} end
  def spaces(i, n, bytes) do
    ## adjust to multiple of 4 
    a = i + rem(4-rem(i,4), 4)
    {n+a, List.foldr(:lists.seq(n, n-1+a, 4), bytes, fn(x,acc) ->   [{x, <<0,0,0,0>>}|acc] end)}
  end
  
  ## The encode function will resolve all labels in the program
  ## code. This is done in two passes, the first (collect) will
  ## register and remove all labels and the second will update branch
  ## instructions and immediate values. The initial labels are
  ## collected from the data segments.
  ##
  ## The program is represented as list of instructions. 
  
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

      ## R-type {op, dest, src, target}

      {:add, rd, rs, rt}    -> <<   @aop::6, rs::5, rt::5, rd::5, 0::5, @add::6>>
      {:sub, rd, rs, rt}    -> <<   @aop::6, rs::5, rt::5, rd::5, 0::5, @sub::6>>


      ## I-type {op, target, src, imm}  ( written "op target imm(src)" in MIPS assembler ) 

      {:addi, rt, rs, imm}  -> <<@addi::6, rs::5, rt::5, immediate(imm, labels)::integer-signed-16>>	

      {:ori,  rt, rs, imm}  -> <<@ori::6, rs::5, rt::5, immediate(imm, labels)::integer-signed-16>>

      {:lb,   rt, rs, imm}  -> <<@lb::6, rs::5, rt::5, immediate(imm, labels)::integer-signed-16>>	

      {:lw,   rt, rs, imm}  -> <<@lw::6, rs::5, rt::5, immediate(imm, labels)::integer-signed-16>>

      {:sb,   rt, rs, imm}  -> <<@sb::6, rs::5, rt::5, immediate(imm, labels)::integer-signed-16>>
			     
      {:sw,   rt, rs, imm}  -> <<@sw::6, rs::5, rt::5, immediate(imm, labels)::integer-signed-16>>

      ## Branch 

      {:beq,  rs, rt, offs} -> <<@beq::6, rs::5, rt::5, offset(offs, addr, labels)::integer-signed-16>>

      {:bne,  rs, rt, offs} -> <<@bne::6, rs::5, rt::5, offset(offs, addr, labels)::integer-signed-16>>			   

      ## extra instructions, not present in the MIPS architecture 
      {:out, rs}            -> <<@out::6, rs::5, 0::5, 0::16>>
      :halt                 -> <<@halt::6, 0::5, 0::5, 0::16>>	
    end
  end
  
  ## If the immediate value is a label it is resolved.

  def immediate(label, labels) when is_atom(label) do
    {_, addr} = List.keyfind(labels, label, 0)
    addr
  end
  def immediate(imm, _labels) do imm end

  ## If the offset value is a label it is resolved.
  
  def offset(label, addr, labels) when is_atom(label) do
    {_, abs} = List.keyfind(labels, label, 0)
    (abs - addr)
  end
  def offset(offs, _labels) do offs end  
    
  

end
