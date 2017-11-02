-module(cache).


%%% The problem with the cache is not adding things or looking up
%%% things but to remove things; the cache should not be able to grow
%%% without control. Items that are no longer valid should of course
%%% be removed, could possibly be removed when the are accessed, but
%%% we should have a maximum number of items in the cache and make
%%% sure that we never execeed this limit. When we want to add a new
%%% item and we have reached the limit we need to remove an
%%% item. Which item to remove is of course the tricky question - the
%%% least used, the one with shortest time to live or something else.


%%% Assuming we want to remove items that have a short time to live
%%% first. We could then order the items in a heap structure ordered
%%% by the time to live. When we add items they are pushed down the
%%% heap and if the uppermost items have expired we can remove them.

%%% A heap ordered by the time to live will of course not give us fast
%%% access so we would need a second datastructure that is ordered
%%% based on the keys. These two structures should then be handled in
%%% a way that we always have a consistent view of the cache.


-compile(export_all).


start() ->
    spawn_link(fun() -> init() end).

init() ->
    Cache = cache:new(),
    Heap = heap:new(),
    cache(Cache, Heap).

cache(Cache, Heap) ->
    receive 
	{lookup, Addr, Ref, From} ->
	    case cache:lookup(Addr, Cache) of
		{Addr, Record} ->
		    From ! {ok, Ref, Record};
		false ->
		    From ! {false, Ref}
	    end,
	    cache(Cache, Heap);		    

	{add, Addr, Record} ->
	    {Expired, Rest} = heap:expired(Heap),
	    Removed = cache:remove(Expired, Cache),
	    Added = cache:add(Addr, Record, Removed),
	    Updated = heap:add(Addr, Record, Rest),
	    cache(Added, Updated)
    end.

	    


	    
    
