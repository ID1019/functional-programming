defmodule Day51 do


  def test_a() do 
    {:puzzle, seeds, maps} = parse(sample())
    locations = locations(seeds, maps)
    Enum.min(locations)
  end  

  def task_a() do
    {:puzzle, seeds, maps} = parse(File.read!("day5.csv"))
    locations = locations(seeds, maps)
    Enum.min(locations)
  end  

  def locations(seeds, maps) do
    List.foldl(maps, seeds, fn(map, nrs) ->
      :io.format(" nrs = ~w~n", [nrs])
      dests(nrs, map) end)
  end

  def dests(nrs, descr) do
    Enum.reduce(descr, nrs, fn(tr, nrs) ->
      Enum.map(nrs, fn(nr) -> tr.(nr) end)
    end)
  end

  def parse(descr) do
    [seeds | maps] = String.split(String.trim(descr), "\n\n")
    [_ | seeds] = String.split(seeds, " ")
    seeds = Enum.map(seeds, fn(str) -> {nr, _} = Integer.parse(String.trim(str)); nr end)
    maps = Enum.map(maps, fn(map) ->
      [_| rows] = String.split(map, "\n")
      Enum.map(rows, fn(row) ->
	[d,s,r] = Enum.map(String.split(row, " "), fn(str) -> {nr, _} = Integer.parse(String.trim(str)); nr end)
	sr = s+r
	ds = d-s
	fn(nr) -> 
	  if s <= nr and nr < sr do
	    ds + nr
	  else
	    nr
	  end
	end
      end)

    end)
    {:puzzle, seeds, maps} 
  end
  


  def sample() do
"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"
  end
    


end
