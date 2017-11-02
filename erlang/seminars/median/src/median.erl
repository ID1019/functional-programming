-module(median).

-compile(export_all).

start() ->
    spawn(fun() -> init() end).

init() ->
    Lower = low_heap(),
    Higher = hi_heap(),
    init(Lower, Higher).

init(Lower, Higher) ->
    receive 
	{median, Pid} ->
	    Pid ! {reply, na},
	    init(Lower, Higher);
	{next, N} ->
	    lower(N, Lower, Higher)
    end.
    

%% add to the lower heap
lower(Med, Lower, Higher) ->
    receive 
	{median, Pid} ->
	    Pid ! {reply, Med},
	    lower(Med, Lower, Higher);
	{next, N} ->
	    if 
		N =< Med ->
		    higher(Med, low_add(N, Lower), Higher);
		true ->
		    {Smallest, Rest} = smalest(Higher),
		    higher(Smallest, low_add(Med, Lower), Rest)
	    end
    end.

%% add to the higher heap
higher(Med, Lower, Higher) ->
    receive 
	{median, Pid} ->
	    Pid ! {reply, Med},
	    lower(Med, Lower, Higher);
	{next, N} ->
	    if 
		N >= Med ->
		    lower(Med, Lower, hi_add(N, Higher));
		true ->
		    {Highest, Rest} = smalest(Lower),
		    lower(Highest, Rest, hi_add(Med, Higher))
	    end
    end.




heap() ->
    nil.

low_add(N, nil) ->
    {heap, N, nil, nil};
low_add(N, {heap, M, L, R}) when N < M ->
    {heap, N, R, low_add(M, L)};
low_add(N, {heap, M, L, R})  ->
    {heap, N, low_add(M, R), L}.



    





		    
		    


		   
