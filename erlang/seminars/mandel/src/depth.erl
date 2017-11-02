-module(depth).

-export([init/0, test/3]).

%%% Ensure that the init() routine is called when module loaded. 

-on_load(init/0).

%%% This init() routine must only be called once.

init() ->
    ok = erlang:load_nif("./depth", 0).

%%% If the loding failed this dummy function is used. 

test(_R, _I, _M) ->
    exit(nif_library_not_loaded).


%%% test(R, I, M) : calculate the mandelbrot value of complex value
%%% {R,I}, with a maximum iteration of M. Returns 0..(M-1). R and I
%%% are floating point values and M is an integer greater than 0. The
%%% depth will be an integer.





