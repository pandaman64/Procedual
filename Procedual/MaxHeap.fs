module MaxHeap

type Leftist<'a when 'a : comparison> =
    Leaf
    | Node of Leftist<'a> * 'a * Leftist<'a> * int

let singleton v = Node(Leaf,v,Leaf,1)

let rank n =
    match n with
    | Leaf -> 0
    | Node(_,_,_,r) -> r 

let rec merge t1 t2 =
    match t1,t2 with
    | Leaf,t | t,Leaf -> t
    | Node(_,v1,_,_),Node(_,v2,_,_) when v1 > v2 -> merge t2 t1
    | Node(l,v1,r,_),_ -> 
        let merged = merge r t2
        let rank_left = rank l
        let rank_right = rank merged
        if rank_left >= rank_right
        then
            Node(l,v1,merged,rank_right + 1)
        else
            Node(merged,v1,l,rank_left + 1)

let insert v n =
    merge n (singleton v)

let tryPeek n =
    match n with
    | Leaf -> None
    | Node(_,v,_,_) -> v

let peek n = tryPeek n |> Option.get

let pop n =
    match n with
    | Leaf -> failwith "empty"
    | Node(l,_,r,_) -> merge l r