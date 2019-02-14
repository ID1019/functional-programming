defmodule PNG_Config do
  @type t :: %PNG_Config{
      width: integer,
      height: integer,
      bit_depth: 8,
      color_type: 2,
      compressionMethod: 0,
      filterMethod: 0,
      interlaceMethod: 0
    }

  @enforce_keys [:width, :height]
  defstruct [
      width: nil,
      height: nil,
      bit_depth: 8, #One byte per R/G/B
      color_type: 2, #RGB triplet
      compressionMethod: 0,
      filterMethod: 0,
      interlaceMethod: 0
    ]
end

defmodule PNG do
  @spec write(binary, list(list(Color.color))) :: :ok
  def write(filename, image_data) do
    config = %PNG_Config{
      height: length(image_data),
      width: length(List.first(image_data))
    }
    write(filename, image_data, config)
  end

  @spec write(binary, list(list(Color.color)), PNG_Config.t) :: :ok
  def write(filename, image_data, config) do
    ioData = [
      header(),
      ihdr(config),
      idats(image_data, config),
      iend()
    ]

    :ok = :file.write_file(filename, ioData)
  end

  @spec header() :: binary
  def header() do
    <<137, 80, 78, 71, 13, 10, 26, 10>>
  end

  @spec ihdr(PNG_Config.t) :: binary
  def ihdr(%PNG_Config{} = config) do
    data = <<
        config.width :: size(32),
        config.height :: size(32),
        config.bit_depth :: size(8),
        config.color_type :: size(8),
        config.compressionMethod :: size(8),
        config.filterMethod :: size(8),
        config.interlaceMethod :: size(8)
      >>
    chunk("IHDR", data)
  end

  @spec idats(list(list(Color.color)), PNG_Config.t) :: binary
  def idats(rows, %PNG_Config{} = config) do
    bit_depth = config.bit_depth
    raw = List.foldl rows, <<>>, fn(row, data) ->
      raw_row = List.foldl row, <<>>, fn({:rgb, r, g, b}, data) ->
        data <> <<
          r :: size(bit_depth),
          g :: size(bit_depth),
          b :: size(bit_depth)
        >>
      end
      data <> <<0 :: size(8)>> <> raw_row
    end
    Enum.map compress(raw), fn(data) ->
      chunk("IDAT", data)
    end
  end

  @spec iend() :: binary
  def iend(), do: chunk("IEND", << >>)

  @spec chunk(binary, binary) :: binary
  def chunk(type, data) do
    <<
      byte_size(data) :: size(32),
      type :: binary,
      data :: binary,
      :erlang.crc32(type <> data) :: size(32)
    >>
  end

  @spec compress(binary) :: list(binary)
  def compress(data) do
    z = :zlib.open()
    :ok = :zlib.deflateInit(z)
    compressed = :zlib.deflate(z, data, :finish)
    :ok = :zlib.deflateEnd(z)
    :zlib.close(z)
    List.flatten compressed
  end
end
