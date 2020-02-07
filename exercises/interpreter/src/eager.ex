defmodule Eager do

  @type atm :: {:atm, atom}
  @type variable :: {:var, atom}
  @type ignore :: :ignore

  @type lambda :: {:lambda, [atom], [atom], seq}

  @type cons(t) :: {:cons, t, t}
  @type expr :: atm | variable | lambda | call | case | cons(expr)

  @type pattern :: atm | variable | ignore | cons(pattern)

  @type match :: {:match, pattern, expr}
  @type seq :: [expr] | [match | seq]

  @type call :: {:call, atom(), [expr]}
  @type clause :: {:clause, pattern, seq}
  @type case :: {:case, expr, [clause]}

  # Expressions are evaluated to structures.
  @type closure :: {:closure, [atom], seq, env}
  @type str :: atom | [str] | closure

  # An environment is a key-value of variableiable to structure.
  @type env :: [{atom, str}]

  # A program is a list of named functions
  @type prgm :: [{atom, [atom], seq}]

  @doc """
  Evaluate a sequence given a program.
  """
  @spec eval(seq, prgm) :: {:ok, str} | :fail

  def eval(seq, prg) do
    # a new environment is created
    eval_seq(seq, Env.new(), prg)
  end

  @doc """
  Evaluate a sequence given an environment and a program. 
  """
  @spec eval_seq([expr], env, prgm) :: {:ok, str} | :error

  def eval_seq([exp], env, prg) do
    eval_expr(exp, env, prg)
  end
  def eval_seq([{:match, ptr, exp} | seq], env, prg) do
    case eval_expr(exp, env, prg) do
      :error ->
        :error

      {:ok, str} ->
	env = eval_scope(ptr, env)

        case eval_match(ptr, str, env) do
          :fail ->
            :error

          {:ok, env} ->
            eval_seq(seq, env, prg)
        end
    end
  end

  @doc """
  Evaluate an expression given an environment and a program. 
  """
  @spec eval_expr(expr, env, prgm) :: {:ok, str} | :error
  def eval_expr({:atm, id}, _, _) do
    {:ok, id}
  end
  def eval_expr({:var, id}, env, _) do
    case Env.lookup(id, env) do
      nil ->
        :error

      {_, str} ->
        {:ok, str}
    end
  end
  def eval_expr({:cons, he, te}, env, prg) do
    case eval_expr(he, env, prg) do
      :error ->
        :error

      {:ok, hs} ->
        case eval_expr(te, env, prg) do
          :error ->
            :error

          {:ok, ts} ->
            {:ok, [hs | ts]}   # what? why not {hs, ts} 
        end
    end
  end
  def eval_expr({:case, expr, cls}, env, prg) do
    case eval_expr(expr, env, prg) do
      :error ->
        :error

      {:ok, str} ->
        eval_cls(cls, str, env, prg)
    end
  end
  def eval_expr({:lambda, par, free, seq}, env, _prg) do
    case Env.closure(free, env) do
      :error ->
        :error

      closure ->
        {:ok, {:closure, par, seq, closure}}
    end
  end
  def eval_expr({:apply, expr, args}, env, prg) do
    case eval_expr(expr, env, prg) do
      :error ->
        :error

      {:ok, {:closure, par, seq, closure}} ->
        case eval_args(args, env, prg) do
          :error ->
            :error

          strs ->
            env = Env.args(par, strs, closure)
            eval_seq(seq, env, prg)
        end
      {:ok, _} ->
	:error
    end
  end
  def eval_expr({:call, id, args}, env, prg) when is_atom(id) do
    case List.keyfind(prg, id, 0) do
      nil ->
        :error

      {_, par, seq} ->
        case eval_args(args, env, prg) do
          :error ->
            :error

          strs ->
            env = Env.args(par, strs, [])
            eval_seq(seq, env, prg)
        end
    end
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

      {_, ^str} ->
        {:ok, env}

      {_, _} ->
        :fail
    end
  end
  def eval_match(:ignore, _, env) do
    {:ok, env}
  end
  def eval_match({:cons, hp, tp}, [hs | ts], env) do
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
  Evaluate a list of clauses given a structure an environment
  and a program. 
  """
  @spec eval_cls(str, [clause], env, prgm) :: {:ok, str} | :error

  def eval_cls([], _, _, _) do
    :error
  end
  def eval_cls([{:clause, ptr, seq} | cls], str, env, prg) do

    env = eval_scope(ptr, env)

    case eval_match(ptr, str, env) do
      :fail ->
        eval_cls(cls, str, env, prg)

      {:ok, env} ->
        eval_seq(seq, env, prg)
    end
  end

  @doc """
  Evaluate a list of expressions, if any expresion evaluates
  to :error then evaluation stops and an :error is returned, otherwise
  a list of the resulting structures is returned.
  """
  @spec eval_args([expr], env, prgm) :: [str] | :error

  def eval_args([], _, _) do [] end
  def eval_args([expr | exprs], env, prg) do
    case eval_expr(expr, env, prg) do
      :error ->
        :error

      {:ok, str} ->
        case eval_args(exprs, env, prg) do
          :error ->
            :error

          strs ->
            [str | strs]
        end
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
