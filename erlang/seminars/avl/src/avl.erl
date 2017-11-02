-module(avl).

-export([tree/0, insert/3, lookup/2,  traverse/1, depth/2, max_depth/1]).

tree() ->
    nil.

insert(Tree, Key, Value) ->
    case insrt(Tree, Key, Value) of
	{inc, Q} ->
	    Q;
	{ok, Q} ->
	    Q
    end.



lookup(nil, _) ->
    fail;
lookup({node, K, V, _, _, _}, K) ->
    {ok, V};
lookup({node, K, _, _, L, R}, Key)  ->
    if
	Key < K -> lookup(L, Key);
	true -> lookup(R, Key)
    end.


depth(nil, _) ->
    fail;
depth({node, K, _, _, _, _}, K) ->
    {ok, 1};
depth({node, K, _, _, L, R}, Key)  ->
    if
	Key < K -> 
	    case depth(L, Key) of
		{ok, D} -> {ok, D + 1};
		fail -> fail
	    end;
	true -> 
	    case depth(R, Key) of
		{ok, D} -> {ok, D + 1};
		fail -> fail
	    end
    end.


max_depth(nil) ->
    0;
max_depth({node, _, _, _, L, R})  ->
    max(max_depth(L), max_depth(R)) + 1.


traverse(nil) ->
    {0,0};
traverse({node, _, _, _, L, R}) ->
    {Lt, Lm} = traverse(L),
    {Rt, Rm} = traverse(R),    
    {Lt+Rt+1, max(Lm, Rm)+1}.



insrt(nil, Key, Value) ->
    %% empty tree
    {inc, {node, Key, Value, 0, nil, nil}};

insrt({node, Key, _, F, A, B}, Key, Value) ->
    %% found in root
    {ok, {node, Key, Value, F, A, B}};


insrt({node, Rk, Rv, 0, A, B}, Kk, Kv)  when Kk < Rk ->
    case insrt(A, Kk, Kv) of
	{inc, Q} ->
	    {inc, {node, Rk, Rv, -1, Q, B}};
	{ok, Q} ->
	    {ok, {node, Rk, Rv, 0, Q, B}}
    end;

insrt({node, Rk, Rv, 0, A, B}, Kk, Kv)  when Kk > Rk ->
    case insrt(B, Kk, Kv) of
	{inc, Q} ->
	    {inc, {node, Rk, Rv, +1, A, Q}};
	{ok, Q} ->
	    {ok, {node, Rk, Rv, 0, A, Q}}
    end;

insrt({node, Rk, Rv, +1, A, B}, Kk, Kv)  when Kk < Rk ->
    case insrt(A, Kk, Kv) of
	{inc, Q} ->
	    {ok, {node, Rk, Rv, 0, Q, B}};
	{ok, Q} ->
	    {ok, {node, Rk, Rv, +1, Q, B}}
    end;

insrt({node, Rk, Rv, -1, A, B}, Kk, Kv)  when Kk > Rk ->
    case insrt(B, Kk, Kv) of
	{inc, Q} ->
	    {ok, {node, Rk, Rv, 0, A, Q}};
	{ok, Q} ->
	    {ok, {node, Rk, Rv, -1, A, Q}}
    end;

insrt({node, Rk, Rv, -1, A, B}, Kk, Kv)  when Kk < Rk ->
    case insrt(A, Kk, Kv) of
	{inc, Q} ->
	    {ok, rotate({node, Rk, Rv, -2, Q, B})};
	{ok, Q} ->
	    {ok, {node, Rk, Rv, -1, Q, B}}
    end;

insrt({node, Rk, Rv, +1, A, B}, Kk, Kv)  when Kk > Rk ->
    case insrt(B, Kk, Kv) of
	{inc, Q} ->
	    {ok, rotate({node, Rk, Rv, +2, A, Q})};
	{ok, Q} ->
	    {ok, {node, Rk, Rv, +1, A, Q}}
    end.


%% left - singel rotate

rotate({node, Xk, Xv, -2, {node, Yk, Yv, -1, A, B}, C}) ->	    
    %% rotate 
    {node, Yk, Yv, 0, A, {node, Xk, Xv, 0, B, C}};

%% right - singel rotate

rotate({node, Xk, Xv, +2, A, {node, Yk, Yv, +1, B, C}}) ->	    
    %% rotate 
    {node, Yk, Yv, 0, {node, Xk, Xv, 0, A, B}, C};


%% left - double rotate 

rotate({node, Xk, Xv, -2, {node, Yk, Yv, +1, A, {node, Zk, Zv, 0, B, C}}, D}) ->	    
    %% double rotate 
    {node, Zk, Zv, 0, {node, Yk, Yv, 0, A, B}, {node, Xk, Xv, 0, C, D}};

rotate({node, Xk, Xv, -2, {node, Yk, Yv, +1, A, {node, Zk, Zv, -1, B, C}}, D}) ->	    
    %% double rotate 
    {node, Zk, Zv, 0, {node, Yk, Yv, 0, A, B}, {node, Xk, Xv, +1, C, D}};

rotate({node, Xk, Xv, -2, {node, Yk, Yv, +1, A, {node, Zk, Zv, +1, B, C}}, D}) ->	    
    %% double rotate 
    {node, Zk, Zv, 0, {node, Yk, Yv, -1, A, B}, {node, Xk, Xv, 0, C, D}};


%% right - double rotate

rotate({node, Xk, Xv, +2, A, {node, Yk, Yv, -1, {node, Zk, Zv, 0, B, C}, D}}) ->	    
    %% double rotate 
    {node, Zk, Zv, 0, {node, Xk, Xv, 0, A, B}, {node, Yk, Yv, 0, C, D}};

rotate({node, Xk, Xv, +2, A, {node, Yk, Yv, -1, {node, Zk, Zv, +1, B, C}, D}}) ->	    
    %% double rotate 
    {node, Zk, Zv, 0, {node, Xk, Xv, -1, A, B}, {node, Yk, Yv, 0, C, D}};

rotate({node, Xk, Xv, +2, A, {node, Yk, Yv, -1, {node, Zk, Zv, -1, B, C}, D}}) ->	    
    %% double rotate 
    {node, Zk, Zv, 0, {node, Xk, Xv, 0, A, B}, {node, Yk, Yv, +1, C, D}}.






