defmodule Depth do 

  ### Ensure that the init() routine is called when module loaded. 

  @on_load :init
  

  ### This init() routine must only be called once.

  def init() do 
    :ok = :erlang.load_nif("./depth", 0)
  end

  @doc """
  The function test(r, i, m) : calculate the mandelbrot value of
  complex value {r,i}, with a maximum iteration of m. Returns a value
  in the range 0..(m-1). The depth needs to be > 0.
  """
  @spec test(float(), float(), integer()) :: integer()

  ### If the loding failed this dummy function is used. 

  def test(_r, _i, _m) do
    exit(:nif_library_not_loaded)
  end

end






