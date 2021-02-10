defmodule High do 

  ## The map adapted to handle streams.
  
  def map({:stream, next}, fun) do
    {:stream,
     fn () -> case next.() do
  		{:ok, from, cont} ->
  		  {:ok, fun.(from), cont}
  		:nil ->
  		  :nil
  	      end
       end
    }
  end

  ##  The first map for ranges.
  def map(range, fun) do
    {:done, res} = reduce(range, {:cont, []}, fn(x,a) -> {:cont, [fun.(x)|a]} end)
    res    
  end

  ## Sum or product of all integers in the range.

  def sum(range) do
    {:done, res} = reduce(range, {:cont, 0}, fn(x,a) -> {:cont, x+a} end)
    res
  end

  def prod(range) do
    {:done, res} = reduce(range, {:cont, 1}, fn(x,a) -> {:cont, x*a} end)
    res
  end  

  ## A list of the first n elements

  def take(range, n) do
    fun = fn (x, {:sofar, s, acc}) ->
      if s == n do
	{:halt, acc}
      else
	{:cont, {:sofar, s+1, [x|acc]}}
      end
    end
    case reduce(range, {:cont, {:sofar, 0, []}}, fun) do
      {:done, {:sofar, _, res}} ->
	res
      {:halted, res} ->
	res
    end
  end

  ## a rewrite where we suspend and also return a continuation

  def toke(range, n) do
    fun = fn (x, {:sofar, s, acc}) ->
      if s == n do
	{:suspend, acc}
      else
	{:cont, {:sofar, s+1, [x|acc]}}
      end
    end
    reduce(range, {:cont, {:sofar, 0, []}}, fun)
  end

  ## A list of all elements less than n
  
  def upto(range, n) do
    reduce(range, {:cont, []},
      fn (x, acc) ->
	if x < n do
	  {:cont, [x|acc]}
	else
	  {:halt, acc}
	end
      end
    )
  end

  ## Sum all integers smaller than n

  def sumto(range, n) do
    reduce(range, {:cont, 0},
      fn (x, acc) ->
	if x < n do
	  {:cont, x+acc}
	else
	  {:halt, acc}
	end
      end
    )
  end

  ## Give me the element in the range that makes the sum excede n.
  
  def last(range, n) do
    reduce(range, {:cont, 0},
      fn (x, s) ->
	if x+s < n do
	  {:cont, s+x}
	else
	  {:halt, x}
	end
      end
    )
  end

  ## Give me the firts element of a stream and a continuation
  
  def head(range) do
    reduce(range,
      {:cont, :na},
      fn (x,_) -> {:suspend, x} end)
  end


  ## An infinite sequence of fibonacci numbers implemented as a stream
  def fib() do
    {:stream, fn() -> fib(1,1) end}
  end

  def fib(f1,f2) do
    {:ok, f1,  fn() -> fib(f2, f1+f2) end}
  end

  ## an infinit sequence of integers implemented as a stream 
  def infinity() do
    {:stream, fn () -> infinity(0) end}
  end

  def infinity(n) do
    {:ok,  n,  fn() -> infinity(n+1) end}
  end

  ## a range implemented as a stream 
  def range(from, to) do
    {:stream, fn() -> next(from, to) end}
  end

  def next(from, to) do
    if from <= to do
      {:ok, from, fn() -> next(from+1, to) end}
    else
      :nil
    end
  end
  

  
  ##  This is the reduce function 

  ##      added to handle streams
  def reduce({:stream, next}, {:cont, acc}, fun) do
    case next.() do
      {:ok, from, cont} ->
	reduce({:stream, cont}, fun.(from,acc), fun)
      :nil ->
	{:done, acc}
    end
  end

  ##     the basic for range
  def reduce({:range, from, to}, {:cont, acc}, fun) do
    if from <= to do
      reduce({:range, from+1, to}, fun.(from,acc), fun)
    else
      {:done, acc}
    end
  end

  ##  added to handle lists
  def reduce([from|rest], {:cont, acc}, fun) do
    reduce(rest, fun.(from,acc), fun)
  end
  def reduce([], {:cont, acc}, _) do
      {:done, acc}
  end

  ## added to be able to suspend, introduce to implement head/1
  def reduce(range, {:suspend, acc}, fun) do
    {:suspended, acc, fn(cmd) -> reduce(range, cmd, fun) end }
  end

  ## added to halt, introduced to implement take/2
  def reduce(_, {:halt, acc}, _) do
    {:halted, acc}
  end


  


end
