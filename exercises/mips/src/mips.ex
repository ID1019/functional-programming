defmodule MIPS do


  def run(prgm) do
    code = Program.assemble(prgm)
    data = Data.new()
    CPU.start(code, data, self())
    collect([])
  end


  def collect(sofar) do
    receive do
      {:alu, :done} ->
	Enum.reverse(sofar)
      {:alu, val} ->
	collect([val|sofar])
    end
  end
  
  
end
