defmodule Controller do

  ## op codes
  @aop     0
  @beq   0x4
  @bne   0x5
  @addi  0x8
  @ori   0xd
  @lb   0x20
  @lw   0x23
  @sb   0x28
  @sw   0x2b

  @out  0x3e
  @halt 0x3f
  
  ## fnct codes 
  @add  0x20
  @sub  0x22

  def start(reg, alu, mem, brn) do
    spawn_link(fn() -> init(reg, alu, mem, brn) end)
  end

  def init(reg, alu, mem, brn) do
    controller(reg, alu, mem, brn)
  end
  
  def controller(reg, alu, mem, brn) do
    receive do
      {:instr, @halt, _} ->
	send(reg, {:ctrl, :halt})
	send(alu, {:ctrl, :halt})
	send(mem, {:ctrl, :halt})
	send(brn, {:ctrl, :halt})
	:io.format("ctr: halt~n", [])
	:ok
      {:instr, op, fnct} ->
	control(op, fnct, reg, alu, mem, brn)
	controller(reg, alu, mem, brn)
    end
  end
  

  def control(op, fnct, reg, alu, mem, brn) do
    case op do 

      ## arithmetic operations
      @aop ->
	:io.format("ctr: alu ~w~n", [fnct])
	send(alu, {:ctrl, fnct, :reg})
	send(mem, {:ctrl, :frw})
	send(reg, {:ctrl, :wrd})
	send(brn, {:ctrl, :nbr})

      # branch
      @beq ->
	:io.format("ctr: beq~n", [])
	send(alu, {:ctrl, @sub, :reg})  # sub
	send(mem, {:ctrl, :frw})
	send(reg, {:ctrl, :nop})
	send(brn, {:ctrl, :beq})

      @bne ->
	:io.format("ctr: bne~n", [])
	send(alu, {:ctrl, @sub, :reg})  # sub
	send(mem, {:ctrl, :frw})
	send(reg, {:ctrl, :nop})
	send(brn, {:ctrl, :bne})

      # addi 
      @addi ->
	:io.format("ctr: addi~n", [])
	send(alu, {:ctrl, @add, :imm})
	send(mem, {:ctrl, :frw})
	send(reg, {:ctrl, :wrt})
	send(brn, {:ctrl, :nbr})

      # ori 
      @ori ->
	:io.format("ctr: addi~n", [])
	send(alu, {:ctrl, @add, :imm})
	send(mem, {:ctrl, :frw})
	send(reg, {:ctrl, :wrt})
	send(brn, {:ctrl, :nbr})

      # load byte lb
      @lb ->
	:io.format("ctr: load byte~n", [])
	send(alu, {:ctrl, @add, :imm})
	send(mem, {:ctrl, :rbyte})
	send(reg, {:ctrl, :wrt})
	send(brn, {:ctrl, :nbr})
	
      # load word lw
      @lw  ->
	:io.format("ctr: load word~n", [])
	send(alu, {:ctrl, @add, :imm})
	send(mem, {:ctrl, :rword})
	send(reg, {:ctrl, :wrt})
	send(brn, {:ctrl, :nbr})

      #store byte sb
      @sb  ->
	:io.format("ctr: store byte~n", [])
	send(alu, {:ctrl, @add, :imm})
	send(mem, {:ctrl, :wbyte})
	send(reg, {:ctrl, :nop})
	send(brn, {:ctrl, :nbr})

      #store word sw
      @sw ->
	:io.format("ctr: store word~n", [])
	send(alu, {:ctrl, @add, :imm})
	send(mem, {:ctrl, :wword})
	send(reg, {:ctrl, :nop})
	send(brn, {:ctrl, :nbr})

      ## Special instructions, not present in MIPS
      # out
      @out  ->
	:io.format("ctr: out~n", [])
	send(alu, {:ctrl, :out})
	send(mem, {:ctrl, :frw})
	send(reg, {:ctrl, :nop})
	send(brn, {:ctrl, :nbr})

      strange ->
	:io.format("ctr: strange ~w~n", [strange])
	:ok
    end
  end
  
end
