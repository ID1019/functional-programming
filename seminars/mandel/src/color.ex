defmodule Color do

  ## Convert a scalar, from 0 to max, to a suitabe color represented as
  ## {:rgb, r ,g ,b} where each element is 0..255. This is just one way of doing
  ## it, there are more advanced ways of doing this so do experiment.

  def convert(d, max) do
    red(d, max)
  end

  def red(d, max) do
    f = d/max
    a = f*4                    ## A is [0 - 4.0]
    x = trunc(a)         ## X is [0,1,2,3,4]
    y = trunc(255*(a-x)) ## Y is [0 - 255]
    case x do
      0 ->
	{:rgb, y, 0, 0};       ## black -> red
      1 ->
	{:rgb, 255, y, 0};     ## red -> yellow
      2 ->
	{:rgb, 255-y,255, 0};  ## yellow -> green
      3 ->
	{:rgb, 0, 255, y};     ## green -> cyan
      4 ->
	{:rgb, 0,255-y,255}    ## cyan -> blue
    end
  end

  def blue(d, max) do
    f = d/max
    a = f*4                    ## A is [0 - 4.0]
    x = trunc(a)         ## X is [0,1,2,3,4]
    y = trunc(255*(a-x)) ## y is [0 - 255]
    case x do
      0 ->
	{:rgb, 0, 0, y}        ## black -> blue
      1 ->
	{:rgb, 0, y, 255}      ## blue -> cyan
      2 ->
	{:rgb, 0, 255, 255-y}  ## cyan -> green
      3 ->
	{:rgb, y, 255, 0}      ## green -> yellow
      4 ->
	{:rgb, 255, 255-y, 0}  ## yellow-> red
    end
  end

end
