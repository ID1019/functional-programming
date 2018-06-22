defmodule HTTP do

  def parse_request(r0) do
    flip()
    {request, r1} = request_line(r0)
    {headers, r2} = headers(r1)
    {body, _} = message_body(r2)
    {request, headers, body}
  end

  
  
  defp request_line([?G, ?E, ?T, 32 | r0]) do
    {uri, r1} = request_uri(r0)
    {ver, r2} = http_version(r1)
    [13, 10 | r3] = r2
    {{:get, uri, ver}, r3}
  end

  defp request_uri([32 | r0]), do: {[], r0}
  defp request_uri([c | r0]) do
    {rest, r1} = request_uri(r0)
    {[c | rest], r1}
  end

  defp http_version([?H, ?T, ?T, ?P, ?/, ?1, ?., ?1 | r0]) do
    {:v11, r0}
  end
  defp http_version([?H, ?T, ?T, ?P, ?/, ?1, ?., ?0 | r0]) do
    {:v10, r0}
  end

  defp headers([13, 10 | r0]), do: {[], r0}
  defp headers(r0) do
    {header, r1} = header(r0)
    {rest, r2} = headers(r1)
    {[header | rest], r2}
  end

  defp header([13, 10 | r0]), do: {[], r0}
  defp header([c | r0]) do
    {rest, r1} = header(r0)
    {[c | rest], r1}
  end

  defp message_body(r), do: {r, []}


  ## The response is as complicated as one whish it to be. We will
  ## keep it simple.


  ##        Response      = Status-Line               ; Section 6.1
  ##                        *(( general-header        ; Section 4.5
  ##                         | response-header        ; Section 6.2
  ##                         | entity-header ) CRLF)  ; Section 7.1
  ##                        CRLF
  ##                        [ message-body ]          ; Section 7.2    


  ##        Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
  
  
  def ok(body) do
    "HTTP/1.1 200 OK\r\n" <>
      "Content-Length: #{String.length(body)}\r\n" <> 
      "\r\n #{body}"
  end

  def fourofour() do
    "HTTP/1.1 404 Not found\r\n" <> "\r\n<html><body><h>404</h1><p>Page not found</p></body></html>."
  end
  
  def get(uri) do
    "GET #{uri} HTTP/1.1\r\n\r\n"
  end


  def flip() do
    if :rand.uniform(100) < 20 do
      Process.exit(self(), "ouch")
    else
      :ok
    end
  end
  
end
