defmodule Color do

  # Convert a scalar, from 0 to max, to a suitabe color
  # represented as {:rgb, r ,g ,b} where each element is
  # 0..255. This is just one way of doing it, there are
  # more advanced ways of doing this so do experiment.
  def convert(d, max) do
    red(d, max)
  end

  def red(d, max) do
    f = d / max

    # a is [0 - 4.0]
    a = f * 4

    # x is [0,1,2,3,4]
    x = trunc(a)

    # y is [0 - 255]
    y = trunc(255 * (a - x))

    case x do
      0 ->
        # black -> red
        {:rgb, y, 0, 0}

      1 ->
        # red -> yellow
        {:rgb, 255, y, 0}

      2 ->
        # yellow -> green
        {:rgb, 255 - y, 255, 0}

      3 ->
        # green -> cyan
        {:rgb, 0, 255, y}

      4 ->
        # cyan -> blue
        {:rgb, 0, 255 - y, 255}
    end
  end

  def blue(d, max) do
    f = d / max

    # a is [0 - 4.0]
    a = f * 4

    # x is [0,1,2,3,4]
    x = trunc(a)

    # y is [0 - 255]
    y = trunc(255 * (a - x))

    case x do
      0 ->
        # black -> blue
        {:rgb, 0, 0, y}

      1 ->
        # blue -> cyan
        {:rgb, 0, y, 255}

      2 ->
        # cyan -> green
        {:rgb, 0, 255, 255 - y}

      3 ->
        # green -> yellow
        {:rgb, y, 255, 0}

      4 ->
        # yellow-> red
        {:rgb, 255, 255 - y, 0}
    end
  end

end
