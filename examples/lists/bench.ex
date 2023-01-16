defmodule Bench do

  def bench(n, l) do
    lst = Enum.to_list(1..l)
    bt = time(n, fn -> :ok end)
    dt = time(n, fn -> dummy(l) end)
    ut = time(n, fn -> union(lst, []) end)
    rt = time(n, fn -> tailr(lst, []) end)

    format("base", n, bt)
    format("dummy", n, dt)    
    format("union", n, ut)    
    format("tailr", n, rt)
  end

  def format(name, n, t) do
    :io.format("~12.10s : ~8.3f\n", [name, t/n])
  end


  def latex(n, l) do
    lst = Enum.to_list(1..l)
    dt = time(n, fn -> dummy(l) end)
    ut = time(n, fn -> union(lst, []) end)
    rt = time(n, fn -> tailr(lst, []) end)

    {:ok, file} = File.open("table.tex", [:write, :list])
    :io.format(file,"\\begin{table}[h]\n", [])
    :io.format(file,"\\begin{center}\n", [])
    :io.format(file,"\\begin{tabular}{l|c|c}\n", []) 
    :io.format(file,"\\textbf{prgm} & \\textbf{runtime} & \\textbf{ratio}\\\\\n", [])
    :io.format(file,"\\hline\n", [])
    latex(file, "dummy", n, dt, dt)    
    latex(file, "union", n, ut, dt)    
    latex(file, "tailr", n, rt, dt)
    :io.format(file, "\\end{tabular}\n", [])
    :io.format(file, "\\caption{Union and friends, list of ~w elements, runtime in microseconds}\n", [l])
    :io.format(file, "\\label{table:union}\n", [])
    :io.format(file, "\\end{center}\n", [])
    :io.format(file, "\\end{table}\n", [])
    File.close(file)
  end

  def latex(file, name, n, t, r) do
    :io.format(file, "~12.10s & ~8.3f & ~8.2f\\\\\n", [name, t/n, t/r])
  end
  


  
  def time(n, f) do
    :erlang.garbage_collect()
    {t,_} =  :timer.tc(fn -> loop(n, f) end)
    t
  end

  def loop(0,_) do :ok end
  def loop(n, f) do
    f.()
    loop(n-1, f)
  end

  
  
  def dummy(0) do :ok end
  def dummy(n) do 
    dummy(n-1)
  end
  
  def union([], y) do y end
  def union([h|t], y) do
    [h | union(t, y)]
  end

  def tailr([], y) do y end
  def tailr([h|t], y) do
    tailr(t, [h | y])
  end    

  def taila([], y) do y end
  def taila([h|t], y) do
    taila(t, y ++ [h])
  end    
  

end
