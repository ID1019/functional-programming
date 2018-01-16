defmodule Env do

  @type value :: any()
  @type var :: atom()
  @type env :: [{var, value}]

  @spec new() :: []
  def new() do
    []
  end

  @spec add(var, value, env) :: env
  def add(id, str, env) do
    [{id, str} | env]
  end

  @spec lookup(var, env) :: value | nil
  def lookup(id, env) do
    List.keyfind(env, id, 0)
  end

  @spec remove([var], env) :: env
  def remove(ids, env) do
    List.foldr(ids, env, fn id, env ->
      List.keydelete(env, id, 0)
    end)
  end

  @spec closure([var], env) :: env | :error
  def closure(ids, env) do
    List.foldr(ids, [], fn id, acc ->
      case acc do
        :error ->
          :error

        cls ->
          case lookup(id, env) do
            {id, value} ->
              [{id, value} | cls]

            nil ->
              :error
          end
      end
    end)
  end

  @spec args([var], [value], env) :: env
  def args(prm, args, env) do
    List.zip([prm, args]) ++ env
  end

end
