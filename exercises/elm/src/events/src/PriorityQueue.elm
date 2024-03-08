module PriorityQueue exposing  (PriorityQueue   -- the queue type, takes one type argumen
                        , new    -- creates an empty queue given a less-than function
                        , empty  -- true or false
                        , rank   -- the rank of the root
                        , size   -- the number of items in the queue
                        , depth  -- the maximum depth 
                        , insert -- adds a new item to the queue
                        , next   -- returns a tuple of head and tail
                        , head   -- returns but does not remove the first item
                        , tail)  -- return a queue where the first item is removed

{-- This implementation uses a "leftists" heap. Each node holds a rank
    that describes the shortest path to en empty leaf. The right
    branch of a node always has lower rank than its left branch.

    Adding and removing are items are both implemented with the merge
    function that maintans the property. The heap becomes fairly balanced.
--}

type alias PriorityQueue item
    = { root : Node item
      , less : item -> item -> Bool
      }

type Node item 
    = Node item Int (Node item) (Node item)
    | Nil

    
new: (item -> item -> Bool) -> PriorityQueue item
new less =
    PriorityQueue Nil less

rank: PriorityQueue item -> Int
rank queue =
    case queue.root of
        Nil -> 0
        Node _ s _ _ -> s


empty: PriorityQueue item -> Bool        
empty queue =
    queue.root == Nil
    
size: PriorityQueue item -> Int
size queue =
    count queue.root

count: Node item -> Int
count nd =
    case nd of
        Nil ->
            0
        Node _ _ left right ->
            1 + (count left) + (count right)

depth: PriorityQueue item -> Int
depth queue =
    maxDepth queue.root

maxDepth: Node item -> Int        
maxDepth node =
    case node of
        Nil -> 0
        Node _ _ left right ->
            (max (maxDepth left) (maxDepth right)) + 1
                   
          
insert: item -> PriorityQueue item -> PriorityQueue item
insert evt queue =
    {queue | root = insertItem queue.less evt queue.root}


insertItem: (item -> item -> Bool) -> item -> Node item -> Node item
insertItem less event queue =
    case queue of
        Nil ->
            Node event 0 Nil Nil
        node ->
           merge less (Node event 0 Nil Nil) node
    
next: PriorityQueue item -> Maybe (item, PriorityQueue item)
next queue =
    case queue.root of
        Nil ->
            Nothing
        Node item _ Nil Nil  ->
            Just (item, {queue | root = Nil})
        Node item _ left Nil  ->
            Just (item, {queue | root = left})
        Node item _ left right  ->
            Just (item, {queue | root = (merge queue.less left right)})

head: PriorityQueue item  -> Maybe item
head queue =
    case queue.root of
        Nil ->
            Nothing
        Node evt _ _ _ ->
            Just evt

tail: PriorityQueue item -> PriorityQueue item
tail queue = 
    case queue.root of
        Nil ->
            {queue | root = Nil}
        Node _ _ Nil Nil  ->
            {queue | root = Nil}
        Node _ _ left Nil  ->
            {queue | root = left}
        Node _ _ left right  ->
            {queue | root =  merge queue.less left right}
                
                    
merge: (item -> item -> Bool) -> Node item -> Node item -> Node item
merge  less left right =
    case right of
        Nil ->
            left
        (Node re _ rl rr ) ->
            case left of
                Nil -> 
                    right
                (Node le _ ll lr) ->
                    if (less le re) then
                        let
                            merged = merge less lr right
                            sm = sval merged
                            sl = sval ll
                            s = (min sm sl) + 1
                        in
                            if (sm < sl) then
                                Node le s ll merged
                            else
                                Node le s merged ll
                    else
                        let
                            merged = merge less rr left
                            sm = sval merged
                            sr = sval rl
                            s = (min sm sr) + 1
                        in
                            if (sm < sr) then
                                Node re s rl merged
                            else
                                Node re s merged rl

  
sval: Node item -> Int                        
sval node = 
    case node of
        Nil ->
            0
        Node _ s _ _ ->
            s
                        
     
