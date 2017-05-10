module MaxHeap

type Leftist<'a,'comp> =
    Leaf
    | Node of Leftist<'a,'comp> * 'a * Leftist<'a,'comp> * int * 'comp

let singleton v comp = Node(Leaf,v,Leaf,1,comp)

let rank n =
    match n with
    | Leaf -> 0
    | Node(_,_,_,r,_) -> r 

let rec merge t1 t2 =
    match t1,t2 with
    | Leaf,t | t,Leaf -> t
    | Node(_,v1,_,_,comp),Node(_,v2,_,_,_) when comp v1 v2 -> merge t2 t1
    | Node(l,v1,r,_,comp),_ -> 
        let merged = merge r t2
        let rank_left = rank l
        let rank_right = rank merged
        if rank_left >= rank_right
        then
            Node(l,v1,merged,rank_right + 1,comp)
        else
            Node(merged,v1,l,rank_left + 1,comp)

let comparator n =
    match n with
    | Leaf -> failwith "not implemented"
    | Node(_,_,_,_,comp) -> comp

let insert v n =
    merge n (singleton v (comparator n))

let tryPeek n =
    match n with
    | Leaf -> None
    | Node(_,v,_,_,_) -> v

let peek n = tryPeek n |> Option.get

let pop n =
    match n with
    | Leaf -> failwith "empty"
    | Node(l,_,r,_,_) -> merge l r