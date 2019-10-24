module E24 exposing (..)

import Array exposing (Array, isEmpty)
import Bitwise
import Char
import Dict exposing (Dict)
import Helpers as H
import Html exposing (..)
import Html.Events exposing (onClick)
import List
import List.Extra
import Parser exposing ((|.), (|=), Parser, andThen, keep, keyword, succeed, symbol)
import Regex
import Set exposing (Set)
import String


type alias GroupProperties =
    { immuneTo : Set String
    , weakTo : Set String
    }


type alias Group =
    { size : Int
    , hp : Int
    , properties : GroupProperties
    , attackDamage : Int
    , attackType : String
    , initiative : Int
    }


parserGroupProperties : Parser GroupProperties
parserGroupProperties =
    Parser.oneOf
        [ Parser.succeed identity
            |. symbol "("
            |= Parser.oneOf
                [ Parser.succeed (,)
                    |. keyword "immune to "
                    |= (keep Parser.oneOrMore (\c -> c /= ')' && c /= ';') |> Parser.map (String.split ", " >> Set.fromList))
                    |= Parser.oneOf
                        [ succeed identity
                            |. keyword "; weak to "
                            |= (keep Parser.oneOrMore (\c -> c /= ')' && c /= ';') |> Parser.map (String.split ", " >> Set.fromList))
                        , succeed Set.empty
                        ]
                , Parser.succeed (,)
                    |. keyword "weak to "
                    |= (keep Parser.oneOrMore (\c -> c /= ')' && c /= ';') |> Parser.map (String.split ", " >> Set.fromList))
                    |= Parser.oneOf
                        [ succeed identity
                            |. keyword "; immune to "
                            |= (keep Parser.oneOrMore (\c -> c /= ')' && c /= ';') |> Parser.map (String.split ", " >> Set.fromList))
                        , succeed Set.empty
                        ]
                    |> Parser.map (\( a, b ) -> ( b, a ))
                ]
            |. symbol ")"
        , succeed ( Set.empty, Set.empty )
        ]
        |> Parser.map (\( a, b ) -> GroupProperties a b)


parserGroup : Parser Group
parserGroup =
    succeed Group
        |= H.pInt
        |. keyword " units each with "
        |= H.pInt
        |. keyword " hit points "
        |= parserGroupProperties
        |. H.spaces
        |. keyword "with an attack that does "
        |= H.pInt
        |. H.spaces
        |= keep Parser.oneOrMore (\c -> Char.isLower c)
        |. keyword " damage at initiative "
        |= H.pInt


type alias State =
    { priorityQueue : PriorityQueue
    }


parserInput : Parser State
parserInput =
    succeed (,)
        |. keyword "Immune System:\n"
        |= Parser.repeat Parser.oneOrMore (succeed identity |= parserGroup |. symbol "\n")
        |. keyword "\nInfection:\n"
        |= Parser.repeat Parser.oneOrMore (succeed identity |= parserGroup |. symbol "\n")
        |> Parser.map
            (\( immunes, infections ) ->
                List.map (PriorityQueueRow Nothing ImmuneSystem) immunes
                    ++ List.map (PriorityQueueRow Nothing Infection) infections
            )
        |> Parser.map (fromList >> State)


type Team
    = ImmuneSystem
    | Infection


type alias PriorityQueueRow =
    { selectedGroup : Maybe Int -- index in the old pq
    , team : Team
    , group : Group
    }


type alias PriorityQueue =
    List PriorityQueueRow


rollback : PriorityQueue -> PriorityQueue -> PriorityQueue
rollback acc pq =
    case acc of
        [] ->
            pq

        p :: rest ->
            rollback rest (p :: pq)


effectivePower : Group -> Int
effectivePower group =
    group.size * group.attackDamage


insert : PriorityQueueRow -> PriorityQueue -> PriorityQueue -> PriorityQueue
insert priorityQueueRow acc priorityQueue =
    case priorityQueue of
        [] ->
            rollback acc [ priorityQueueRow ]

        p1 :: rest ->
            if
                effectivePower p1.group
                    > effectivePower priorityQueueRow.group
                    || (effectivePower p1.group == effectivePower priorityQueueRow.group && p1.group.initiative > priorityQueueRow.group.initiative)
            then
                insert priorityQueueRow (p1 :: acc) rest
            else
                rollback acc (priorityQueueRow :: p1 :: rest)


fromList : List PriorityQueueRow -> PriorityQueue
fromList priorityQueueRowList =
    priorityQueueRowList
        |> List.sortBy (.group >> (\g -> ( 0 - effectivePower g, 0 - g.initiative )))


damage : Group -> Group -> Int
damage attacker defender =
    if defender.properties.immuneTo |> Set.member attacker.attackType then
        0
    else
        let
            damage_ =
                effectivePower attacker
        in
        if defender.properties.weakTo |> Set.member attacker.attackType then
            damage_ * 2
        else
            damage_


selectOne : Set Int -> PriorityQueue -> PriorityQueueRow -> Maybe Int
selectOne chosen all attacker =
    all
        |> List.indexedMap (,)
        |> List.foldl
            (\( i, pqr ) ->
                if attacker.team /= pqr.team && not (Set.member i chosen) && damage attacker.group pqr.group > 0 then
                    Dict.insert ( 0 - damage attacker.group pqr.group, 0 - effectivePower pqr.group, 0 - pqr.group.initiative ) i
                else
                    identity
            )
            Dict.empty
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.map Tuple.second


targetSelection : Set Int -> PriorityQueue -> PriorityQueue -> List PriorityQueueRow -> PriorityQueue
targetSelection chosen all todo acc =
    case todo of
        [] ->
            acc |> fromList

        pqr :: rest ->
            case selectOne chosen all pqr of
                Nothing ->
                    targetSelection chosen all rest ({ pqr | selectedGroup = Nothing } :: acc)

                Just i ->
                    let
                        insertOrCrash i =
                            if Set.member i chosen then
                                Debug.crash ""
                            else
                                chosen |> Set.insert i
                    in
                    targetSelection (insertOrCrash i) all rest ({ pqr | selectedGroup = Just i } :: acc)


attack : Group -> Group -> Group
attack attacker defender =
    let
        damage_ =
            damage attacker defender

        hpLeft =
            defender.size * defender.hp - damage_

        sizeLeft =
            -- if hpLeft <= 0 then
            --     0
            -- else
            --     ((hpLeft - 1) // defender.hp) + 1
            defender.size - (damage_ // defender.hp)
    in
    { defender
        | size =
            if sizeLeft <= 0 then
                0
            else
                sizeLeft
    }


attackPhase : PriorityQueue -> PriorityQueue
attackPhase priorityQueue =
    let
        sortedOnInitiative =
            priorityQueue |> List.indexedMap (,) |> List.sortBy (Tuple.second >> .group >> .initiative >> (*) -1)

        _ =
            sortedOnInitiative |> List.map (\( i, pqr ) -> ( i, pqr ))

        dict_ =
            priorityQueue |> List.indexedMap (\i pqr -> ( i, pqr )) |> Dict.fromList
    in
    sortedOnInitiative
        |> List.foldl
            (\( i, pqr ) dict ->
                let
                    attacker =
                        dict |> Dict.get i |> H.uM |> .group
                in
                if attacker.size <= 0 then
                    dict
                else
                    case pqr.selectedGroup of
                        Nothing ->
                            dict

                        Just defenderI ->
                            dict
                                |> Dict.update defenderI
                                    (\mdefenderPqr ->
                                        let
                                            defenderPqr =
                                                mdefenderPqr |> H.uM
                                        in
                                        { defenderPqr | group = attack attacker defenderPqr.group } |> Just
                                    )
            )
            dict_
        |> Dict.values


fight : State -> State
fight state =
    targetSelection Set.empty state.priorityQueue state.priorityQueue []
        |> attackPhase
        |> List.map (\x -> { x | selectedGroup = Nothing })
        |> List.filter (\x -> x.group.size > 0)
        |> fromList
        |> State


fightUntilDefeat : State -> State
fightUntilDefeat state =
    let
        onlyInfections =
            state.priorityQueue |> List.filter (\pqr -> pqr.team == Infection)

        onlyImmuneSystems =
            state.priorityQueue |> List.filter (\pqr -> pqr.team == ImmuneSystem)

        newState =
            fight state

        alive s =
            s.priorityQueue |> List.map (.group >> .size) |> List.sum
    in
    if 0 == List.length onlyInfections || 0 == List.length onlyImmuneSystems then
        state
    else if alive state /= alive newState then
        fightUntilDefeat (fight state)
    else
        state


findBoost : Int -> State -> State
findBoost i state =
    let
        newState =
            state.priorityQueue
                |> List.map
                    (\pqr ->
                        case pqr.team of
                            ImmuneSystem ->
                                let
                                    g =
                                        pqr.group
                                in
                                { pqr | group = { g | attackDamage = g.attackDamage + i } }

                            _ ->
                                pqr
                    )
                |> fromList
                |> State

        newFinalState =
            fightUntilDefeat newState

        hasInfections =
            newFinalState.priorityQueue |> List.filter (\pqr -> pqr.team == Infection) |> List.length |> (<) 0
    in
    if not hasInfections then
        newFinalState
    else
        let
            _ =
                Debug.log "add" i
        in
        findBoost (i + 1) state


test1 =
    input
        |> Parser.run parserInput
        |> H.uR
        |> Debug.log "state "
        |> fightUntilDefeat
        |> Debug.log "state after defeat"
        |> .priorityQueue
        |> List.map (.group >> .size)
        |> List.sum
        |> Debug.log "result1"


test2 =
    input
        |> Parser.run parserInput
        |> H.uR
        |> Debug.log "state "
        |> findBoost 0
        |> Debug.log "result"
        |> .priorityQueue
        |> List.map (.group >> .size)
        |> List.sum
        |> Debug.log "result2"


inpute =
    """Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
"""


inputtest =
    """Immune System:
16 units each with 100 hit points (weak to radiation, bludgeoning) with an attack that does 110 fire damage at initiative 2
16 units each with 100 hit points (weak to radiation, bludgeoning) with an attack that does 110 fire damage at initiative 1
16 units each with 100 hit points (weak to radiation, bludgeoning) with an attack that does 110 fire damage at initiative 3

Infection:
4 units each with 600 hit points (weak to radiation) with an attack that does 200 bludgeoning damage at initiative 1
"""


inputtest1 =
    """Immune System:
1 units each with 2 hit points (weak to dupa, chuj; immune to kutas) with an attack that does 3 super damage at initiative 1
5 units each with 10 hit points (immune to kutas; weak to dupa, chuj) with an attack that does 3 super damage at initiative 1
3 units each with 13 hit points (immune to kutasiwo) with an attack that does 3 super damage at initiative 1
6 units each with 12 hit points (weak to pedal) with an attack that does 3 super damage at initiative 1

Infection:
4 units each with 600 hit points with an attack that does 200 bludgeoning damage at initiative 1
"""


input =
    """Immune System:
76 units each with 3032 hit points with an attack that does 334 radiation damage at initiative 7
4749 units each with 8117 hit points with an attack that does 16 bludgeoning damage at initiative 16
4044 units each with 1287 hit points (immune to radiation, fire) with an attack that does 2 fire damage at initiative 20
1130 units each with 11883 hit points (weak to radiation) with an attack that does 78 radiation damage at initiative 14
1698 units each with 2171 hit points (weak to slashing, fire) with an attack that does 11 bludgeoning damage at initiative 12
527 units each with 1485 hit points with an attack that does 26 bludgeoning damage at initiative 17
2415 units each with 4291 hit points (immune to radiation) with an attack that does 17 cold damage at initiative 5
3266 units each with 6166 hit points (immune to cold, slashing; weak to radiation) with an attack that does 17 bludgeoning damage at initiative 18
34 units each with 8390 hit points (immune to cold, fire, slashing) with an attack that does 2311 cold damage at initiative 10
3592 units each with 5129 hit points (immune to cold, fire; weak to radiation) with an attack that does 14 radiation damage at initiative 11

Infection:
3748 units each with 11022 hit points (weak to bludgeoning) with an attack that does 4 bludgeoning damage at initiative 6
2026 units each with 11288 hit points (weak to fire, slashing) with an attack that does 10 slashing damage at initiative 13
4076 units each with 23997 hit points (immune to cold) with an attack that does 11 bludgeoning damage at initiative 19
4068 units each with 40237 hit points (immune to cold; weak to slashing) with an attack that does 18 slashing damage at initiative 4
3758 units each with 16737 hit points (weak to slashing) with an attack that does 6 radiation damage at initiative 2
1184 units each with 36234 hit points (weak to bludgeoning, fire; immune to cold) with an attack that does 60 radiation damage at initiative 1
1297 units each with 36710 hit points (immune to cold) with an attack that does 47 fire damage at initiative 3
781 units each with 18035 hit points (immune to bludgeoning, slashing) with an attack that does 36 fire damage at initiative 15
1491 units each with 46329 hit points (immune to slashing, bludgeoning) with an attack that does 56 fire damage at initiative 8
1267 units each with 34832 hit points (immune to cold) with an attack that does 49 radiation damage at initiative 9
"""
