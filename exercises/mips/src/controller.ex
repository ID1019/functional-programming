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

      {:instr, @out, _} ->
	send(alu, {:ctrl, :out})
	send(mem, {:ctrl, :frw})
	send(reg, {:ctrl, :nop})
	send(brn, {:ctrl, :nbr})
	controller(reg, alu, mem, brn)

      {:instr, op, fnct} ->
	{fnct, a, m, r, b} = control(op, fnct)
	send(alu, {:ctrl, fnct, a})
	send(mem, {:ctrl, m})
	send(reg, {:ctrl, r})
	send(brn, {:ctrl, b})
	controller(reg, alu, mem, brn)
    end
  end

  def control(op, fnct) do
    case op do 

      ## arithmetic operations
      @aop ->
	:io.format("ctr: alu ~w~n", [fnct])
	{fnct, :reg, :frw, :wrd, :nbr}
      # branch
      @beq ->
	:io.format("ctr: beq~n", [])
	{@sub, :reg, :frw, :nop, :beq}

      @bne ->
	:io.format("ctr: bne~n", [])
	{@sub, :reg, :frw, :nop, :bne} 

      # addi 
      @addi ->
	:io.format("ctr: addi~n", [])
	{@add, :imm, :frw, :wrt, :nbr}

      # ori 
      @ori ->
	:io.format("ctr: addi~n", [])
	{@add, :imm, :frw, :wrt, :nbr}

      # load byte lb
      @lb ->
	:io.format("ctr: load byte~n", [])
	{@add, :imm, :rbyte, :wrt, :nbr}	
	
      # load word lw
      @lw  ->
	:io.format("ctr: load word~n", [])
	{@add, :imm, :rword, :wrt, :nbr}	

      #store byte sb
      @sb  ->
	:io.format("ctr: store byte~n", [])
	{@add, :imm, :wbyte, :nop, :nbr}	

      #store word sw
      @sw ->
	:io.format("ctr: store word~n", [])
	{@add, :imm, :wword, :nop, :nbr}	

      strange ->
	:io.format("ctr: strange ~w~n", [strange])
	:ok
    end
  end
  
end
