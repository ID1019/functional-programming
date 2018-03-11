defmodule Church do

  # A Church number n is a function of two arguments that
  # applies the first argument n times to its second argument.
  def to_church(0) do
    fn(_), y -> y end
  end
  def to_church(n) do
    fn(f, x) -> f.(to_church(n - 1).(f, x)) end
  end

  # We can convert a Church number to an integer using the
  # function fn(x) -> x + 1 end and let the Church number
  # apply this to zero. 
  def to_integer(church) do
    church.(fn(x) -> 1 + x end, 0)
  end

  # The successor of n is a functon that takes two arguments 
  # and applies the first argument to the second n times
  # usng the Church number and the adds another application.
  def succ(n) do
    fn(f, x) -> f.(n.(f, x)) end
  end

  # Adding n and m is acheived by returning a function that
  # takes two argumnets and then using n and m to apply the
  # first argumnet to the second n plus m times.
  def add(n, m) do
    fn(f, x) -> n.(f, m.(f, x)) end
  end

  # Multiplying n and m; return a function that takes two
  # arguments where the first argument will be included in
  # a function that applies it m times to an argumnet, and
  # this functhen then is applied n times on teh second
  # argumnet x.
  def mul(n, m) do
    fn(f, x) -> n.(fn(y) -> m.(f, y) end, x) end
  end

  # The predecessor of n is tricky but you have to know
  # that in Church numbers the predecessor of zero is zero
  # i.e. there are no negative numbers.
  def pred(n) do
    fn(f, x) ->
      ( n.(                                    # n is a Church numeral
         fn(g) -> fn(h) -> h.(g.(f)) end end,  #  apply this function n times 
         fn(_) -> x end)                       #  to this function 
      ).(fn(u) -> u end)                       # apply it to the identity function
    end
  end

  ## How does this work? Assume we call pred(four) where 
  ## four is the Church numeral for 4. Then we will return
  ## a function, call it three, that as expected takes two
  ## argumnets, a function f and and argumnet x. What
  ## happens if we now use this function and call three.(i, 0)? 
  ##
  ## First we will call four, with a strange looking 
  ## function and a function that ignores its argument.
  ## Let's apply this functions ones, what do we get?
  ##
  ##    fn(h) -> h.( (fn(_) -> 0 end).(i)) end 
  ##
  ## or
  ##
  ##    fn(h) -> h.(0) end
  ## 
  ## Ok, so now we have three more goes to apply the
  ## strange function.
  ##
  ##    fn(h) -> h.( (fn(h') -> h'.(0) end).(i)) end 
  ##
  ## or
  ##  
  ##   fn(h) -> h.( i.(0) ) end     
  ##
  ## Hmmm, let's give it another shot:
  ##
  ##   fn(h) -> h.( (fn(h') -> h'.(i.(0)) end).(i)) end 
  ##
  ## or
  ##  
  ##   fn(h) -> h.(f.(f.(0)) ) end 
  ##
  ## Ok, a last time:
  ##
  ##   fn(h) -> h( fn(h') -> h'.(f.(f.(0))) end).(f) end 
  ##
  ## or
  ##  
  ##   fn(h) -> h.(f.(f.(f.(0)))) end 
  ## 
  ## And now we take this function and apply it to the
  ## identity function fn(u) -> u end. The result is:
  ##
  ##    f.(f.(f.(0)))
  ##
  ## and this is of course execty what we would like to 
  ## see from (pred(four)).(f, 0). 
  ##
  ## If you think this was complicated you're right!

  # At least this lets us define minus.  
  def minus(m, n) do
    n.(fn(x) -> pred(x) end, m)
  end

  # Warning - this will not work since if_then_else will
  # evaluate all arguments. 
  def manos(m, n) do
    if_then_else(is_zero(n), m, manos(m, pred(n)))
  end

  # This works but now we use the Elixir case statement and
  # thereby avoid the eager evaluation. Using case is of 
  # course cheating. 
  def menos(m, n) do
    case to_bool(is_zero(n)) do
      true ->
        m

      false ->
        menos(pred(m), pred(n))
    end
  end

  # Boolean values are encoded as follows:
  def church_true() do
    fn(x, _) -> x end
  end

  def church_false() do
    fn(_, y) -> y end
  end

  # And can be converted by:
  def to_bool(bool) do
    bool.(true, false)
  end

  ## If then else ... but it will evaluate all arguments!
  def if_then_else(i, t, e) do
    i.(t, e)
  end

  ## Boolean operators are so easy to implement.
  def church_and(a, b) do
    a.(b, a)
  end

  def church_or(a, b) do
    a.(a, b)
  end

  def church_not(a) do
    fn x, y -> a.(y, x) end
  end

  # Predicates
  def is_zero(n) do
    n.(fn(_) -> church_false() end, church_true())
  end

  def leq(m, n) do
    is_zero(minus(m, n))
  end

end
