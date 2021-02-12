defmodule Env do

  @type value :: any()
  @type key :: atom()
  @type env :: [{key, value}]

  @spec new() :: []

  def new() do
    []
  end

  @spec add(key, value, env) :: env

  def add(key, str, env) do
    [{key, str} | env]
  end

  @spec lookup(key, env) :: {key, value} | nil

  def lookup(key, env) do
    List.keyfind(env, key, 0)
  end

  @spec remove([key], env) :: env

  def remove(keys, env) do
    List.foldr(keys, env, fn(key, env) ->
      List.keydelete(env, key, 0)
    end)
  end

  @spec closure([key], env) :: env | :error

  def closure(keyss, env) do
    List.foldr(keyss, [], fn(key, acc) ->
      case acc do
        :error ->
          :error

        cls ->
          case lookup(key, env) do
            {key, value} ->
              [{key, value} | cls]

            nil ->
              :error
          end
      end
    end)
  end

  @spec args([key], [value], env) :: env

  def args(pars, args, env) do
    List.zip([pars, args]) ++ env
  end

end
