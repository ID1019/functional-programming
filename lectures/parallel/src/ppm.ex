defmodule PPM do

  ## Stream processing
  
  def writer(file, out) do
    spawn_link(fn() -> write_init(file, out) end)
  end

  def reader(file, out) do
    spawn_link(fn() -> reader_init(file, out) end)
  end    

    
  def write_init(file, out) do
    {:ok, fd} = File.open(file, [:charlist,:write])
    receive do
      {:header, {p, size, depth}} ->
	write_header(p, size, depth, fd)
	write_lines(p, fd)
	send(out, :done)
	File.close(fd)
    end
  end

  def reader_init(file, out) do
    {:ok, fd} = File.open(file, [:binary, :read])
    {:ok, p, {width, height}, depth} = read_header(fd) 
    send(out, {:header, {p, {width, height}, depth}})
    read_lines(height, width, p, fd, out)
    send(out,:done)
    File.close(fd)
  end
    
  def write_lines(p, fd) do
    receive do
      {:line, line} ->
	:ok = write_line(p, line, fd)
	write_lines(p, fd)
      :done ->
	:ok
    end
  end

  def read_lines(0, _width, _p, _fd, _out) do :ok end
  def read_lines(row, width, p, fd, out) do
    send(out, {:line, read_line(p, width, fd)})
    read_lines(row-1, width, p, fd, out)
  end

  ## Batch processing

  def read(file) do
    {:ok, fd} = File.open(file, [:binary, :read])
    {:ok, p, {width, height}, depth} = read_header(fd) 
    lines  = read_lines(height, width, p, fd)
    File.close(fd)
    {:image, {p, {width, height}, depth}, lines}
  end

  def write({:image,  {p, size, depth}, lines}, file) do
    {:ok, fd} = File.open(file, [:charlist,:write])
    write_header(p, size, depth, fd)
    write_lines(lines, p, fd)
    File.close(fd)
    :ok
  end
  
  
  def read_lines(0, _width, _p, _fd) do [] end
  def read_lines(row, width, p, fd) do
    [read_line(p, width, fd)| read_lines(row-1, width, p, fd)]
  end

  def write_lines([], _, _) do  :ok  end
  def write_lines([line|lines], p, fd) do
    :ok = write_line(p, line, fd)
    write_lines(lines, p, fd)
  end  
  
  ## shared functional part, used by both stream and batch interface

  def read_header(fd) do
    {:ok, p} = read_type(fd)
    {:ok, comments} = read_comments([], fd)    
    {:ok, {width, height}} = read_size(fd)
    {:ok, comments} = read_comments(comments, fd)
    {:ok, depth} = read_depth(fd)
    {:ok, _comments} = read_comments(comments, fd)
    {:ok, p, {width, height}, depth}
  end


  def write_header(p, size, depth, fd) do
    write_type(p, fd)
    write_size(size, fd)
    write_depth(depth, fd)
  end

  def read_line(p, width, fd) do
    decode(p, :erlang.binary_to_list(IO.binread(fd, bytes(p, width))))
  end

  def write_line(p, line, fd) do
    IO.write(fd, encode(p, line))
  end
  
  def bytes(:rgb, width) do width*3 end
  def bytes(:gray, width) do width end
  def bytes(:bw, width) do div(width, 8) end
  

  ## Encoding and decoding RGB, gray scale or b/w images.

  def encode(:rgb, line) do rgb_to_chars(line) end
  def encode(:gray, line) do  gray_to_chars(line) end
  def encode(:bw, line) do bw_to_chars(line) end

  def rgb_to_chars(line) do
    List.foldr(line, [], fn({r,g,b}, acc) -> [r, g, b | acc] end)
  end

  def gray_to_chars(line) do
    line
  end

  def bw_to_chars([]) do  [] end
  def bw_to_chars([a1,a2,a3,a4,a5,a6,a7,a8|line]) do
    [<<a1::1,a2::1,a3::1,a4::1,a5::1,a6::1,a7::1,a8::1>>| bw_to_chars(line)]
  end


  def decode(:rgb, chars) do chars_to_rgb(chars) end
  def decode(:gray, chars) do chars_to_gray(chars) end
  def decode(:bw, chars) do chars_to_bw(chars) end


  def chars_to_rgb([]) do [] end
  def chars_to_rgb([r,g,b|line]) do
    [{r,g,b} | chars_to_rgb(line)]
  end


  def chars_to_gray(chars) do chars end

  def chars_to_bw([<<a1::1,a2::1,a3::1,a4::1,a5::1,a6::1,a7::1,a8::1>>|chars]) do
    [a1,a2,a3,a4,a5,a6,a7,a8|chars_to_bw(chars)]
  end


  ## Reading and writing header information

  def read_type(fd) do
    case IO.read(fd, :line) do
      "P6\n" -> {:ok, :rgb}
      "P5\n" -> {:ok, :gray}
      "P4\n" -> {:ok, :bw}
      _ -> :error
    end
  end
  

  def read_comments(comments, fd) do
    case IO.read(fd, 1) do
      <<?\#>> ->
	read_comments([IO.read(fd, :line)|comments], fd)
      _ ->
	:file.position(fd, {:cur, -1})
	{:ok, comments}
    end
  end
  

  def read_size(fd) do
    [width, height] = String.split(IO.read(fd,:line), [" "], trim: true)
    {:ok, {String.to_integer(String.trim(width)), String.to_integer(String.trim(height))}}
  end


  def read_depth(fd) do
    {:ok, String.to_integer(String.trim( IO.read(fd, :line)))}
  end


  def write_type(p, fd) do
    case p do
      :rgb ->
	IO.write(fd, 'P6\n');
      :gray ->
	IO.write(fd, 'P5\n');
      :bw ->
	IO.write(Fd, 'P4\n')
    end
  end

  def write_comment([], _fd) do :ok end
  def write_comment([comment|comments], fd) do
    IO.write(fd, '#')
    IO.write(fd, comment)
    write_comment(comments, fd)
  end

  def write_size({w,h}, fd) do
    IO.write(fd, Integer.to_charlist(w) ++ ' ' ++ Integer.to_charlist(h) ++ '\n')
  end

  def write_depth(depth, fd) do
    IO.write(fd, Integer.to_charlist(depth) ++ "\n")
  end

end
