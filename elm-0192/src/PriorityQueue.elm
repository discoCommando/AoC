module PriorityQueue exposing (PriorityQueue, isEmpty, pop, push, singleton)

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



--type alias PriorityQueue a =
--    { q : Dict Int (List a), f : a -> Int, smallest : List Int }
--
--
--singleton : a -> (a -> Int) -> PriorityQueue a
--singleton a f =
--    PriorityQueue (Dict.empty |> Dict.insert (f a) [ a ]) f [ f a ]
--
--
--push : a -> PriorityQueue a -> PriorityQueue a
--push a p =
--    { p
--        | q =
--            p.q
--                |> Dict.update (p.f a)
--                    (\ml ->
--                        case ml of
--                            Nothing ->
--                                Just [ a ]
--
--                            Just l ->
--                                Just (l ++ [ a ])
--                    )
--    }
--
--
--pushHelper : (a -> Int) -> a -> List a -> List a
--pushHelper f a l =
--    case l of
--        [] ->
--            [ a ]
--
--        first :: rest ->
--            if f a < f first then
--                a :: first :: rest
--
--            else
--                first :: pushHelper f a rest
--
--
--type IsEmpty a
--    = IsEmpty ( a, PriorityQueue a )
--
--
--isEmpty : PriorityQueue a -> Maybe (IsEmpty a)
--isEmpty pq =
--    case Dict.isEmpty pq.q of
--        True ->
--            Nothing
--
--        False ->
--            Just (IsEmpty ( first, { pq | q = rest } ))
--
--
--pop : IsEmpty a -> ( a, PriorityQueue a )
--pop (IsEmpty ( head, rest )) =
--    ( head, rest )
