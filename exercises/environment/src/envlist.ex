defmodule EnvList do

  def new() do [] end

  def add([], key, value) do  [{key,value}] end
  def add([{key,_}|map], key, value) do [{key,value}|map] end
  def add([ass|map], key, value) do
    [ass|add(map, key,value)]
  end

  def lookup([], _key) do  nil  end
  def lookup([{key,_}=ass|_], key) do ass end
  def lookup([_|map], key) do lookup(map, key) end

  def remove([], _key) do  []  end
  def remove([{key,_}|map], key) do map end
  def remove([kv|map], key) do [kv|remove(map, key)] end
  
end
