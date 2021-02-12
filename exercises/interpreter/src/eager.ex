defmodule Eager do

  @type atm :: {:atm, atom}
  @type variable :: {:var, atom}
  @type ignore :: :ignore
  @type cons(t) :: {:cons, t, t}

  # Pattern matching
  @type pattern :: atm | variable | ignore | cons(pattern)  
  
  @type lambda :: {:lambda, [atom], [atom], seq}
  @type apply :: {:apply, expr, [expr]}  
  @type case :: {:case, expr, [clause]}
  @type clause :: {:clause, pattern, seq}

  @type expr :: atm | variable | lambda | apply | case | cons(expr)

  # Sequences
  @type match :: {:match, pattern, expr}
  @type seq :: [expr] | [match | seq]
  
  # Expressions are evaluated to structures.
  @type closure :: {:closure, [atom], seq, env}
  @type str :: atom | [str] | closure

  # An environment is a key-value of variableiable to structure.
  @type env :: [{atom, str}]

  @doc """
  Evaluate a sequence given a program.
  """
  @spec eval(seq) :: {:ok, str} | :fail

  def eval(seq) do
    # a new environment is created
    eval_seq(seq, Env.new())
  end

  @doc """
  Evaluate a sequence given an environment and a program. 
  """
  @spec eval_seq([expr], env) :: {:ok, str} | :error

  def eval_seq([exp], env) do
    eval_expr(exp, env)
  end
  def eval_seq([{:match, ptr, exp} | seq], env) do
    case eval_expr(exp, env) do
      :error ->
        :error

      {:ok, str} ->
	env = eval_scope(ptr, env)

        case eval_match(ptr, str, env) do
          :fail ->
            :error

          {:ok, env} ->
            eval_seq(seq, env)
        end
    end
  end

  @doc """
  Evaluate an expression given an environment and a program. 
  """
  @spec eval_expr(expr, env) :: {:ok, str} | :error
  def eval_expr({:atm, id}, _) do
    {:ok, id}
  end
  def eval_expr({:var, id}, env) do
    case Env.lookup(id, env) do
      nil ->
	IO.puts("variable binding for #{id} not present")
        :error

      {_, str} ->
        {:ok, str}
    end
  end
  def eval_expr({:cons, he, te}, env) do
    case eval_expr(he, env) do
      :error ->
        :error

      {:ok, hs} ->
        case eval_expr(te, env) do
          :error ->
            :error
          {:ok, ts} ->
            {:ok, {hs , ts}}   
        end
    end
  end
  def eval_expr({:case, expr, cls}, env) do
    case eval_expr(expr, env) do
      :error ->
        :error

      {:ok, str} ->
        eval_cls(cls, str, env)
    end
  end
  def eval_expr({:lambda, par, free, seq}, env) do
    case Env.closure(free, env) do
      :error ->
        :error

      closure ->
        {:ok, {:closure, par, seq, closure}}
    end
  end
  def eval_expr({:apply, expr, args}, env) do
    case eval_expr(expr, env) do
      :error ->
        :error

      {:ok, {:closure, par, seq, closure}} ->
        case eval_args(args, env) do
          :error ->
            :error

          {:ok, strs} ->
            env = Env.args(par, strs, closure)
            eval_seq(seq, env)
        end
      {:ok, _} ->
	:error
    end
  end

  def eval_expr({:fun, id}, _env) do 
    {par, seq} = apply(Prgm, id, []) 
    {:ok, {:closure, par, seq, []}}
  end

  def eval_expr(strange, _) do
    IO.puts("strange expresion: ")
    IO.inspect(strange)
    :error
  end

  @doc """
  Evaluate a match of a pattern and structure given an environment
  """
  @spec eval_match(pattern, str, env) :: {:ok, env} | :fail

  def eval_match({:atm, id}, id, env) do
    {:ok, env}
  end
  def eval_match({:var, id}, str, env) do
    case Env.lookup(id, env) do
      nil ->
        {:ok, Env.add(id, str, env)}

      {^id, ^str} ->
        {:ok, env} 

      {_, _} ->
        :fail
    end
  end
  def eval_match(:ignore, _, env) do
    {:ok, env}
  end
  def eval_match({:cons, hp, tp}, {hs, ts}, env) do
    case eval_match(hp, hs, env) do
      :fail ->
        :fail

      {:ok, env} ->
        eval_match(tp, ts, env) 
    end
  end
  def eval_match(_, _, _) do
    :fail
  end

  @doc """
  Create a new scope, remove all variables in the pattern
  from the given environment.
  """
  @spec eval_scope(pattern, env) :: env

  def eval_scope(ptr, env) do
    Env.remove(extract_vars(ptr), env)
  end
  
  
  @doc """
  Evaluate a list of clauses given a structure an environment.
  """
  @spec eval_cls([clause], str, env) :: {:ok, str} | :error

  def eval_cls([], _, _) do
    IO.puts("no more clauses")
    :error
  end
  def eval_cls([{:clause, ptr, seq} | cls], str, env) do

    env = eval_scope(ptr, env)
    case eval_match(ptr, str, env) do
      :fail ->
        eval_cls(cls, str, env)

      {:ok, env} ->
        eval_seq(seq, env)
    end
  end

  @doc """
  Evaluate a list of expressions, if any expresion evaluates
  to :error then evaluation stops and an :error is returned, otherwise
  a list of the resulting structures is returned.
  """
  @spec eval_args([expr], env) :: {:ok, [str]} | :error

  def eval_args(args, env) do
    eval_args(args, env, [])
  end
  
  def eval_args([], _, strs) do {:ok, Enum.reverse(strs)}  end
  def eval_args([expr | exprs], env, strs) do
    case eval_expr(expr, env) do
      :error ->
        :error
      {:ok, str} ->
        eval_args(exprs, env, [str|strs]) 
    end
  end

  @spec extract_vars(pattern) :: [variable]

  def extract_vars(pattern) do
    extract_vars(pattern, [])
  end

  @spec extract_vars(pattern, [variable]) :: [variable]

  def extract_vars({:atm, _}, vars) do vars end
  def extract_vars(:ignore, vars) do vars end
  def extract_vars({:var, var}, vars) do
    [var | vars]
  end
  def extract_vars({:cons, head, tail}, vars) do
    extract_vars(tail, extract_vars(head, vars))
  end

end
