-module(test).

-compile(export_all).



test() ->
  M0 = mem:new([a,b,c,d,e,f]),
  M1 = mem:write(M0, 3, foo),
  M2 = mem:write(M1, 5, bar),
  V2 = mem:read(M2, 2),
  V5  = mem:read(M2, 5),
  {it_is_so_easy, V2, V5}.
