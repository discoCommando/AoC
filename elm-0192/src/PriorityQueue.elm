module PriorityQueue exposing (PriorityQueue, empty, isEmpty, pop, push, singleton)

--import Dict exposing (Dict)


type alias Value a =
    { value : a, orderingValue : Int }


type alias NodeState a =
    { rank : Int, value : Value a, left : PriorityQueue a, right : PriorityQueue a }


type PriorityQueue a
    = Empty
    | Node (NodeState a)


merge : PriorityQueue a -> PriorityQueue a -> PriorityQueue a
merge h1 h2 =
    case h1 of
        Empty ->
            h2

        Node nodeState1 ->
            case h2 of
                Empty ->
                    h1

                Node nodeState2 ->
                    if nodeState1.value.orderingValue <= nodeState2.value.orderingValue then
                        makeT nodeState1.value nodeState1.left (merge nodeState1.right h2)

                    else
                        makeT nodeState2.value nodeState2.left (merge h1 nodeState2.right)


rank : PriorityQueue a -> Int
rank heap =
    case heap of
        Empty ->
            0

        Node ns ->
            ns.rank


makeT : Value a -> PriorityQueue a -> PriorityQueue a -> PriorityQueue a
makeT x a b =
    if rank a >= rank b then
        Node { rank = rank b + 1, value = x, left = a, right = b }

    else
        Node { rank = rank a + 1, value = x, left = b, right = a }


push : a -> Int -> PriorityQueue a -> PriorityQueue a
push a i h =
    merge (singleton a i) h


isEmpty : PriorityQueue a -> Maybe (NodeState a)
isEmpty h =
    case h of
        Empty ->
            Nothing

        Node ns ->
            Just ns


pop : NodeState a -> ( a, PriorityQueue a )
pop ns =
    ( ns.value.value, merge ns.left ns.right )


singleton : a -> Int -> PriorityQueue a
singleton a i =
    Node { rank = 1, value = Value a i, left = Empty, right = Empty }


empty : PriorityQueue a
empty =
    Empty
