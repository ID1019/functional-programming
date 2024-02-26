defmodule Day55 do


  def test_b() do 
    {:puzzle, seeds, maps} = parse(sample())
    locations = locations(seeds, maps)
    Ranges.min(locations)
  end  

  def task_b() do
    {:puzzle, seeds, maps} = parse(File.read!("day5.csv"))
    locations = locations(seeds, maps)
    Ranges.min(locations)
  end  

  def locations(ranges, maps) do
    List.foldl(maps, ranges, fn(map, rngs) ->
      ##:io.format(" rngs = ~w~n", [rngs])
      dests(rngs, map) end)
  end

  def dests(ranges, descr) do
    {rest, transf} =  List.foldl(descr, {ranges, Ranges.empty()}, fn(tr, {rng,upd}) -> transf(rng, upd, tr) end)
    Ranges.union(rest, transf)
  end

  def transf(rng, upd, {:tr, d, r}) do
    int = Ranges.intersection(rng, r)
    rst = Ranges.difference(rng, int)
    upd = Ranges.union(Ranges.add(int, d), upd)
    {rst, upd}
  end  
  
  def parse(descr) do
    [seeds | maps] = String.split(String.trim(descr), "\n\n")
    [_ | seeds] = String.split(seeds, " ")
    seeds = Enum.map(seeds, fn(str) -> {nr, _} = Integer.parse(String.trim(str)); nr end)
    seeds = Enum.map(Enum.chunk_every(seeds ,2), fn([s,r]) -> Ranges.range(s,s+r-1) end)
    seeds = List.foldl(seeds, Ranges.empty, fn(rng, acc) -> Ranges.union(rng, acc) end)
    maps = Enum.map(maps, fn(map) ->
      [_| rows] = String.split(map, "\n")
      Enum.map(rows, fn(row) ->
	[d,s,r] = Enum.map(String.split(row, " "), fn(str) -> {nr, _} = Integer.parse(String.trim(str)); nr end)
	
	{:tr, (d-s), Ranges.range(s,s+r-1)}
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
