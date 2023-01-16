defmodule HTTP3 do

  def parse(r0) do
    body = fn(r2, request, headers) ->
      body(r2, fn(_, body) -> {request, headers, body} end)
    end

    headers = fn(r1, request) ->
      headers(r1, fn(r2, headers) -> body.(r2, request, headers) end)
    end    

    request = fn(r0) ->
      request(r0, headers)
    end

    request.(r0)

  end

  def request(r0, f) do
    case request(r0) do
      {:ok, {request, r1}} ->
	f.(r1, request)
      :more ->
	{:more, fn(more) -> request(r0 ++ more, f) end}
    end
  end

  def headers(r0, f) do
    case headers(r0) do
      {:ok, {headers, r1}} ->
	f.(r1, headers)
      :more ->
	{:more, fn(more) -> headers(r0 ++ more, f) end}
    end
  end

  def body(r0,f) do
    case body(r0) do
      {:ok, {body, r1}} ->
	f.(r1, body)
      :more ->
	{:more, fn(more) -> body(r0++more, f) end}
    end
  end
  
	
  
  
  
  def request([?G, ?E, ?T, 32 | r0]) do
    case uri(r0) do 
    {:ok, {uri, r1}} -> case  version(r1) do
		   {:ok, {ver, r2}} -> case r2 do
				  [13, 10 | r3] ->
				    {:ok, {{:get, uri, ver}, r3}}
				  _ ->
				    :more
				end
		   :more ->
		     :more
		 end
      :more ->
	:more
    end
  end
  
  def uri([]) do :more end
  def uri([32 | r0]), do: {:ok, {[], r0}}
  def uri([c | r0]) do
    case uri(r0) do
    {:ok, {rest, r1}} -> 
	{:ok, {[c | rest], r1}}
      :more ->
	:more
    end
  end

  
  
  def version([?H, ?T, ?T, ?P, ?/, ?1, ?., ?1 | r0]) do
    {:ok, {:v11, r0}}
  end
  def version([?H, ?T, ?T, ?P, ?/, ?1, ?., ?0 | r0]) do
    {:ok, {:v10, r0}}
  end
  def http_version(_) do :more end


	
  def headers([]), do: :more 
  def headers([13, 10 | r0]), do: {:ok, {[], r0}}
  def headers(r0) do
    case  header(r0) do
      {:ok, {header, r1}} ->
	case headers(r1) do
	  {:ok, {headers, r2}} ->
	    {:ok, {[header | headers], r2}}
	  :more ->
	    :more
	end
      :more ->
	:more
    end
  end

  
  
  def header([13, 10 | r0]), do: {:ok, {[], r0}}
  def header([c | r0]) do
    case header(r0) do
      {:ok, {rest, r1}} ->
	{:ok, {[c | rest], r1}}
      :more ->
	:more
    end
  end
  def header(_) do :more end
  
  
  def body(r), do: {:ok, {r, []}}

  

end  
