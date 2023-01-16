defmodule Cards do



  def first( card1, card2) do
    if elem(card1,1) == elem(card2,1) do
      elem(card1,2) > elem(card2,2)
    else

      if (elem(card1,1) == :spade) do
	if (elem(card2,1) == :heart) || (elem(card2,1) == :club) || (elem(card2,1) == :dimond) do
	  true
	else 
	  false
	end
      else

	if (elem(card1,1) == :heart) do
	  if (elem(card2,1) == :club) || (elem(card2,1) == :dimond) do
	    true
	  else
	    false
	  end
	else

	  if (elem(card1,1) == :club) do
	    if (elem(card2,1) == :dimond) do
	      true
	    else 
	    false
	    end
	  else
	    false
	  end
	end
      end
    end
  end
  

  def second(card1, card2) do
    cond do
      elem(card1,1) == elem(card2,1) -> elem(card1, 2) > elem(card2, 2)

      (elem(card1,1) == :spade) -> 
	if (elem(card2,1) == :heart) || (elem(card2,1) == :club) || (elem(card2,1) == :dimond) do
	  true
	else 
	  false
	end

      (elem(card1,1) == :heart) ->       
         if (elem(card2,1) == :club) || (elem(card2,1) == :dimond) do
	   true
	 else
	   false
	 end

      (elem(card1,1) == :club) ->       
        if (elem(card2,1) == :dimond) do
  	   true
	else 
	false
	end

      true ->
	false
    end
  end


  def third(card1, card2) do
    case card1 do
      {:card, :spade, v1} -> 
	case card2 do
	  {:card, :spade, v2} -> v1 > v2
	  {:card, :heart, _} -> true
	  {:card, :club, _} -> true
	  {:card, :dimond, _} -> true
	end
      {:card, :heart, v1} -> 
	case card2 do
	  {:card, :spade, _} -> false
	  {:card, :heart, v2} -> v1 > v2
	  {:card, :club, _} -> true
	  {:card, :dimond, _} -> true
	end
      {:card, :club, v1} -> 
	case card2 do
	  {:card, :spade, _} -> false
	  {:card, :heart, _} -> false
	  {:card, :club, v2} ->  v1 > v2
	  {:card, :dimond, _} -> true
	end
      {:card, :dimond, v1} -> 
	case card2 do
	  {:card, :spade, _} -> false
	  {:card, :heart, _} -> false
	  {:card, :club, _} ->  false
	  {:card, :dimond, v2} -> v1 > v2
	end
    end
  end

  def fourth(card1, card2) do

    {:card, suite2, v2} = card2
    
    case card1 do
      {:card, ^suite2, v1} -> v1 > v2

      {:card, :spade, _} -> 
	case card2 do
	  {:card, :heart, _} -> true
	  {:card, :club, _} -> true
	  {:card, :dimond, _} -> true
	end
      {:card, :heart, _} -> 
	case card2 do
	  {:card, :spade, _} -> false
	  {:card, :club, _} -> true
	  {:card, :dimond, _} -> true
	end
      {:card, :club, _} -> 
	case card2 do
	  {:card, :spade, _} -> false
	  {:card, :heart, _} -> false
	  {:card, :dimond, _} -> true
	end
      {:card, :dimond, _} -> 
	case card2 do
	  {:card, :spade, _} -> false
	  {:card, :heart, _} -> false
	  {:card, :club, _} ->  false
	end
    end
  end



  def fifth({:card, suite1, v1},  {:card, suite2, v2}) do

    case suite1 do
      ^suite2 -> v1 > v2

      :spade-> 
	case suite2 do
	  :heart -> true
	  :club -> true
	  :dimond -> true
	end
      :heart-> 
	case suite2 do
	  :spade -> false
	  :club -> true
	  :dimond -> true
	end
      :club-> 
	case suite2 do
	  :spade -> false
	  :heart -> false
	  :dimond -> true
	end
      :dimond -> 
	case suite2 do
	  :spade -> false
	  :heart -> false
	  :club ->  false
	end
    end
  end

  def sixth({:card, suite, v1},  {:card, suite, v2}) do  v1 > v2 end

  def sixth({:card, :spade, _},  {:card, _, _}) do true end

  def sixth({:card, :heart, _},  {:card, :club, _}) do true end
  def sixth({:card, :heart, _},  {:card, :dimond, _}) do true end

  def sixth({:card, :club, _},  {:card, :dimond, _}) do true end  

  def sixth({:card, _ , _},  {:card, _ , _}) do false end
    
  
  
end
