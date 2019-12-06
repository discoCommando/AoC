module Queue exposing (Queue, StepResult(..), push, singleton, step)


type alias Queue a =
    List a


pop :
    Queue a
    -> ( a, Queue a ) -- unsafe pop from the beginning
pop q =
    case q of
        [] ->
            Debug.todo "pop"

        s :: rest ->
            ( s, rest )


singleton : a -> Queue a
singleton =
    List.singleton


push : a -> Queue a -> Queue a
push a =
    (++) >> (|>) (singleton a)


size : Queue a -> Int
size =
    List.length


type StepResult a r
    = Finish r
    | Next (Queue a)


step : (a -> Queue a -> StepResult a r) -> Queue a -> StepResult a r
step f q =
    stepHelper (size q) f q


stepHelper : Int -> (a -> Queue a -> StepResult a r) -> Queue a -> StepResult a r
stepHelper length f q =
    if length == 0 then
        Next q
    else
        let
            ( a, nq ) =
                pop q
        in
        case f a nq of
            Finish r ->
                Finish r

            Next q2 ->
                stepHelper (length - 1) f q2
