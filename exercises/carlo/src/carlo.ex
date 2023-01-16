defmodule Carlo do  
  
  def rounds(k, r)  do
    a = round(1000, r, 0)
    rounds(k, 1000, r, a)
  end
  
  def rounds(0, n, _, a) do 4*a/n end
  def rounds(k, n, r, a) do
    a = round(n, r, a)
    n = n*2
    pi = 4*a/n
    :io.format(" n = ~12w, pi = ~14.10f,  dp = ~14.10f, da = ~8.4f,  dz = ~12.8f\n", [n, pi,  (pi - :math.pi()), (pi - 22/7), (pi - 355/113)])
    rounds(k-1, n, r, a)
  end
  

  def round(0, _, a) do a end
  def round(k, r, a) do
    if dart(r) do
      round(k-1, r, a+1)
    else
      round(k-1, r, a)	
    end
  end
  
  
  def dart(r) do 
    x= :rand.uniform(r)
    y = :rand.uniform(r)
    #x = Enum.random(0..r)
    #y = Enum.random(0..r)
    :math.pow(r,2) > (:math.pow(x,2) + :math.pow(y,2))
  end

  ## Suming the Leibnitz theorem will rapidly give us a good aproximation.
  
  def leibnitz(n) do
    4 * Enum.reduce(0..n, 0, fn(k,a) -> a + 1/(4*k + 1) - 1/(4*k + 3) end)
    end

  ## Track the arch satrting in teh upper left corner. In each step we
  ## move one position in x and probe from the last y value to the next.

  def track(r) do
    {_, sum} = Enum.reduce(1..r, {r,0}, fn(x, {y, a}) -> y = probe(y,x,r); {y, a+y} end)
    4 * (sum/:math.pow(r,2))
  end

  def probe(0, _x, _r) do 0 end
  def probe(y, x, r) do
    if :math.pow(r,2) > (:math.pow(x,2) + :math.pow(y,2)) do
      y
    else
      probe(y-1,x,r)
    end
  end
  
  

end
