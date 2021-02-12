defmodule Meta  do

  ## macor that allows definition of functions
  
  defmacro fun(head, do: body), do: do_def(head, body)

  defp do_def({name, hl, parms}, body) do    
    param = encode_parms(parms)
    seq = encode_body(body)
    {{:., [], [{:__aliases__, [alias: false], [:Kernel]}, :def]},
     [], [{name, hl, []}, [do: {:__block__, [], [{param, seq}]}]]}
  end

  ## transform before evaluate
  ## Meta.eval((x = :a; x))
  
  defmacro eval(body) do
    seq = encode_body(body)
    quote do Eager.eval(unquote(seq)) end
  end

  ## transform a sequence 
  ##  Meta.encode((x = :a; x))
  
  defmacro encode(body) do
    seq = encode_body(body)
    quote do unquote(seq) end
  end
  

  ## encoding Elixir AST as Meta AST
  
  def encode_body({:__block__, _, body}) do
    encode_seq(body)
  end
  def encode_body(expr) do
    [encode_expr(expr)]
  end  
  
  def encode_parms(parms), do: Enum.map(parms, fn({var, _, nil}) -> var end)

  def encode_seq([expr]), do: [encode_expr(expr)]
  def encode_seq([match | seq]) do
    [encode_match(match) | encode_seq(seq)]
  end
  
  def encode_match({:=, _, [pattern, expr]}) do
    {:{}, [], [:match, encode_pattern(pattern) , encode_expr(expr)]}
  end

  def encode_pattern({:_, _, nil}) do
    :ignore
  end
  def encode_pattern({name, _, nil}) do
    {:{}, [], [:var, name]}
  end
  def encode_pattern(atm) when is_atom(atm) do
    {:{}, [], [:atm, atm]}    
  end
  def encode_pattern({p1, p2}) do
    {:{}, [], [:cons, encode_pattern(p1), encode_pattern(p2)]}
  end
  def encode_pattern(pattern) do
    IO.inspect(pattern)
    :pattern
  end      


  def encode_expr(atm) when is_atom(atm) do
    {:{}, [], [:atm, atm]}    
  end
  def encode_expr({e1, e2}) do
    {:{}, [], [:cons, encode_expr(e1), encode_expr(e2)]}
  end
  def encode_expr({:fn, _, [ {:->, _, [parms, body]}]}) do
    {:{}, [], [:lambda, encode_parms(parms), [], encode_body(body)]}
  end
  def encode_expr({:case, _, [ expr, [do: clauses]]}) do
    {:{}, [], [:case, encode_expr(expr), encode_clauses(clauses)]}
  end

  def encode_expr({{:., _,  [expr]}, _, args}) do
    {:{}, [], [:apply, encode_expr(expr), encode_args(args)]}
  end

  def encode_expr({name, _, nil}) do
    {:{}, [], [:var, name]}  
  end
  def encode_expr({name, _, args}) when is_list(args) do
    {:{}, [], [:apply, {:fun, name}, encode_args(args)]}  
  end  
  
  def encode_expr(expr) do
    IO.inspect(expr)
    :expr
  end      

  def encode_args(args) do
    Enum.map(args, fn(arg) -> encode_expr(arg) end)
  end
  
  
  def encode_clauses(clauses) do
    Enum.map(clauses,
      fn ({:->, _, [[pattern], body]}) ->
	{:{}, [], [:clause, encode_pattern(pattern), encode_body(body)]}
      end)
  end
 
end

