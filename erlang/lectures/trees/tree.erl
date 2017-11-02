-module(tree).

-compile(export_all).

tree() ->
    {node, k, 38, 
     {node, b, 34, nil, nil},
     {node, o, 40, 
      {node, l, 39, nil, nil},
      {node, p, 42, nil, nil}}}.




delete(Key, {node, Key, _, nil, nil}) ->
    nil;
delete(Key, {node, Key, _, Left, nil}) ->
    Left;
delete(Key, {node, Key, _, nil, Right}) ->
    Right;
delete(Key, {node, Key, _, Left, Right}) ->
    {R, V} = rightmost(Left),
    Deleted = delete(R, Left),
    {node, R, V, Deleted, Right};
delete(Key, {node, K, V, Left, Right}) ->
    if 
	Key < K ->
	    {node, K, V, delete(Key, Left), Right};
	true ->
	    {node, K, V, Left, delete(Key, Right)}
    end.

rightmost({node, Key, Value, _, nil}) ->
    {Key, Value};
rightmost({node, _, _, _, Right}) ->
    rightmost(Right).


