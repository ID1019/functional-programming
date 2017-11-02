-module(splay).

-export([tree/0, update/3, lookup/2]).

tree() ->
    nil.

%%% In splay trees we will in each update operation move the key value
%%% pair to the root of teh tree. We will try to keep the braches
%%% equal in length but since the tree is ordered and the updated pair
%%% should be in the root the tree will sometimes be unballanced. The
%%% average tree will though be fairly well balanced. This will keep
%%% operations in O(lg(n)) time komplexity. 


%%% The root - we have two special cases and two general cases. The
%%% general cases will use splay/2 to update a branch in the
%%% tree. The result from updating the branch will change the upper
%%% layer of the tree.

update(nil, Key, Value) ->
    %%  The special case when the tree is empty
    {node, Key, Value, nil, nil};

update({node, Key, _, A, B}, Key, Value) ->
    %% We've found what we're looking for and it is at the root.
    {node, Key, Value, A, B};

update({node, Rk, Rv, Zig, C}, Key, Value) when Key < Rk ->
    %% The general rule where we will do the Zig transformation
    {splay, _, A, B} = splay(Zig, Key),
    {node, Key, Value, A, {node, Rk, Rv, B, C}};

update({node, Rk, Rv, A, Zag}, Key, Value) when Key >= Rk ->
    %% The general rule where we will do the Zag transformation
    {splay, _, B, C} = splay(Zag, Key),
    {node, Key, Value, {node, Rk, Rv, A, B}, C}.


%%% This is the heart of the splay operation.  We will always return a
%%% tuple {splay, Value, L, R} where L and R are subtrees. If The Key
%%% is not found a 'na' value is returned.

splay(nil, _) ->
    %%  The special case when the tree is empty
    {splay, na, nil, nil};

splay({node, Key, Value, A, B}, Key) ->
    %% We've found what we're looking for.
    {splay, Value, A, B};

splay({node, Rk, Rv, nil, B}, Key) when  Key < Rk->
    %% Should go left, but the left branch empty.
    {splay, na, nil, {node, Rk, Rv, nil, B}};

splay({node, Rk, Rv, A, nil}, Key) when  Key >= Rk->
    %% Should go right, but the right branch empty.
    {splay, na, {node, Rk, Rv, A, nil}, nil};

splay({node, Rk, Rv, {node, Key, Value, A, B}, C}, Key)   ->
    %% Found to the left 
    {splay, Value, A, {node, Rk, Rv, B, C}};

splay({node, Rk, Rv, A, {node, Key, Value, B, C}}, Key)  ->
    %% Found to the right
    {splay, Value, {node, Rk, Rv, A, B}, C};


%% Follows the general rules where we have the zig-zag patterns.

splay({node, Gk, Gv, {node, Pk, Pv, ZigZig, C} , D}, Key) when ((Key < Gk) and (Key < Pk))  ->
    %% Going down left-left, this is the so called zig-zig case. 
    {splay, Value, A, B}  = splay(ZigZig, Key),
    {splay, Value, A, {node, Pk, Pv, B, {node, Gk, Gv, C, D}}};

splay({node, Gk, Gv, {node, Pk, Pv, A, ZigZag} , D}, Key) when ((Key < Gk) and (Key >= Pk)) ->
    %%  Going down left-right, this is the so called zig-zag case. 
    {splay, Value, B, C} =  splay(ZigZag, Key),
    {splay, Value, {node, Pk, Pv, A, B}, {node, Gk, Gv, C, D}};

splay({node, Gk, Gv, A, {node, Pk, Pv, ZagZig, D}}, Key) when ((Key >= Gk) and (Key < Pk))  ->
    %%  Going down right-left, this is the so called zag-zig case. 
    {splay, Value, B, C}  =  splay(ZagZig, Key),
    {splay, Value, {node, Gk, Gv, A, B}, {node, Pk, Pv, C, D}};

splay({node, Gk, Gv, A, {node, Pk, Pv, B, ZagZag}}, Key) when ((Key >= Gk) and (Key >= Pk)) ->
    %%  Going down right-right, this is the so called zag-zag case. 
    {splay, Value, C, D} =  splay(ZagZag, Key),
    {splay, Value, {node, Pk, Pv, {node, Gk, Gv, A, B}, C}, D}.



%%% Same thing but now we will only do a lookup. The lookup will still
%%% change the structure of the tree.

lookup(nil, _) ->
    %%  The special case when the tree is empty
    fail;

lookup({node, Key, Value, A, B}, Key) ->
    %% We've found what we're looking for and it is at the root.
    {ok, Value, {node, Key, Value, A, B}};

lookup({node, Rk, Rv, Z, C}, Key) when Key < Rk ->
    case splay(Z, Key) of
	{splay, na, _, _} ->
	    fail;
	{splay, Value, A, B} ->	
	    {ok, Value, {node, Key, Value, A, {node, Rk, Rv, B, C}}}
    end;

lookup({node, Rk, Rv, A, Z}, Key) when Key >= Rk ->
    case splay(Z, Key) of
	{splay, na, _, _} ->
	    fail;
	{splay, Value, B, C} ->
	    {ok, Value, {node, Key, Value, {node, Rk, Rv, A, B}, C}}
    end.








	








