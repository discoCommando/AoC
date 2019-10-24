module E15 exposing (..)

import Array exposing (Array)
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


type Tile
    = Wall
    | OpenSpace


type alias Board =
    Dict ( Int, Int ) Tile


type alias Heroes =
    Dict ( Int, Int ) HeroState


type Hero
    = Elf
    | Goblin


type alias HeroState =
    { health : Int
    , hero : Hero
    }


initialHealth =
    200


type alias State =
    { board : Board, heroes : Heroes, roundCount : Int }


parseLine : String -> Int -> State -> State
parseLine string y state =
    string
        |> String.toList
        |> List.foldl
            (\c ( state, x ) ->
                case c of
                    '#' ->
                        ( { state | board = state.board |> Dict.insert ( y, x ) Wall }, x + 1 )

                    '.' ->
                        ( { state | board = state.board |> Dict.insert ( y, x ) OpenSpace }, x + 1 )

                    'E' ->
                        ( { state
                            | board = state.board |> Dict.insert ( y, x ) OpenSpace
                            , heroes = state.heroes |> Dict.insert ( y, x ) (HeroState initialHealth Elf)
                          }
                        , x + 1
                        )

                    'G' ->
                        ( { state
                            | board = state.board |> Dict.insert ( y, x ) OpenSpace
                            , heroes = state.heroes |> Dict.insert ( y, x ) (HeroState initialHealth Goblin)
                          }
                        , x + 1
                        )

                    _ ->
                        ( state, x + 1 )
            )
            ( state, 0 )
        |> Tuple.first


getAdjacentOpenSpace : ( Int, Int ) -> State -> List ( Int, Int )
getAdjacentOpenSpace ( y, x ) state =
    [ ( y - 1, x ), ( y, x - 1 ), ( y, x + 1 ), ( y + 1, x ) ]
        |> List.filterMap
            (\p ->
                case Dict.get p state.board |> Maybe.withDefault Wall of
                    Wall ->
                        Nothing

                    OpenSpace ->
                        if Dict.member p state.heroes then
                            Nothing
                        else
                            Just p
            )


getAdjacentEnemies : ( Int, Int ) -> Hero -> State -> List ( Int, Int )
getAdjacentEnemies ( y, x ) hero state =
    [ ( y - 1, x ), ( y, x - 1 ), ( y, x + 1 ), ( y + 1, x ) ]
        |> List.filterMap
            (\p ->
                case Dict.get p state.heroes of
                    Nothing ->
                        Nothing

                    Just otherHero ->
                        if otherHero.hero == hero then
                            Nothing
                        else
                            Just p
            )


getEnemies : Hero -> State -> List ( Int, Int )
getEnemies hero state =
    let
        heroes =
            state.heroes
                |> Dict.toList
                |> List.filter (Tuple.second >> .hero >> (/=) hero)
                |> List.map Tuple.first
    in
    heroes


possiblePointsHelper : ( Int, Int ) -> List ( Int, Int ) -> State -> List ( ( Int, Int ), List ( Int, Int ) ) -> Dict ( Int, Int ) ( Int, List ( Int, Int ) ) -> Dict ( Int, Int ) ( Int, List ( Int, Int ) )
possiblePointsHelper point pathSoFar state pointsToGo points =
    let
        adjacent =
            getAdjacentOpenSpace point state
    in
    case Dict.get point points of
        Just _ ->
            case pointsToGo of
                [] ->
                    points

                ( first, psf ) :: rest ->
                    possiblePointsHelper first psf state rest points

        Nothing ->
            let
                newPoints =
                    Dict.insert point ( List.length pathSoFar + 1, point :: pathSoFar ) points

                newPointsToGo =
                    pointsToGo ++ (adjacent |> List.map (\p -> ( p, point :: pathSoFar )))
            in
            case newPointsToGo of
                [] ->
                    newPoints

                ( first, psf ) :: rest ->
                    possiblePointsHelper first psf state rest newPoints



-- pointsToCheck |> List.foldl (\p -> possiblePointsHelper p state (length + 1) )


getPossiblePoints : ( Int, Int ) -> State -> Dict ( Int, Int ) ( Int, List ( Int, Int ) )
getPossiblePoints point state =
    possiblePointsHelper point [] state [] Dict.empty |> Dict.remove point


move : (Hero -> Int) -> ( Int, Int ) -> State -> State
move hit point state =
    let
        hero =
            state.heroes |> Dict.get point
    in
    case hero of
        Nothing ->
            state

        Just hero ->
            let
                adjacent =
                    getAdjacentOpenSpace point state
            in
            case adjacent of
                [] ->
                    attack hit point state

                _ ->
                    case getAdjacentEnemies point hero.hero state of
                        _ :: _ ->
                            attack hit point state

                        -- state
                        _ ->
                            let
                                enemies =
                                    getEnemies hero.hero state

                                adjacents =
                                    enemies
                                        |> List.map (\p -> getAdjacentOpenSpace p state)
                                        |> List.concat

                                possiblePointsToGo =
                                    getPossiblePoints point state

                                closest =
                                    adjacents
                                        |> List.filterMap (\p -> Dict.get p possiblePointsToGo |> Maybe.map (\( l, path ) -> ( p, l, path )))
                                        |> List.sortBy (\( p, _, _ ) -> p)
                                        |> List.sortBy (\( _, l, _ ) -> l)
                            in
                            case closest of
                                [] ->
                                    state

                                ( _, _, pathReversed ) :: _ ->
                                    let
                                        newPoint =
                                            pathReversed |> List.reverse |> H.at 1
                                    in
                                    { state | heroes = state.heroes |> Dict.remove point |> Dict.insert newPoint hero }
                                        |> attack hit newPoint



-- hit : Hero -> Int
-- hit _ =
--     3


attack : (Hero -> Int) -> ( Int, Int ) -> State -> State
attack hit point state =
    let
        hero =
            state.heroes |> Dict.get point
    in
    case hero of
        Nothing ->
            state

        Just hero ->
            let
                enemies =
                    getAdjacentEnemies point hero.hero state
                        |> List.map (\p -> ( p, Dict.get p state.heroes |> H.uM ))
                        |> List.sortBy Tuple.first
                        |> List.sortBy (Tuple.second >> .health)
            in
            case enemies of
                [] ->
                    state

                ( enemyPoint, enemy ) :: _ ->
                    let
                        updatedEnemy =
                            { enemy | health = enemy.health - hit hero.hero }
                    in
                    if updatedEnemy.health <= 0 then
                        { state | heroes = state.heroes |> Dict.remove enemyPoint }
                    else
                        { state | heroes = state.heroes |> Dict.insert enemyPoint updatedEnemy }


step : (Hero -> Int) -> State -> State
step hit state =
    let
        heroesInitialPoints =
            state.heroes |> Dict.toList |> List.map Tuple.first
    in
    heroesInitialPoints |> List.foldl (\point state -> move hit point state) { state | roundCount = state.roundCount + 1 }


drawBoard : State -> String
drawBoard state =
    let
        mappedBoard =
            state.board
                |> Dict.map
                    (\point tile ->
                        case Dict.get point state.heroes of
                            Nothing ->
                                case tile of
                                    Wall ->
                                        '#'

                                    OpenSpace ->
                                        '.'

                            Just hero ->
                                case hero.hero of
                                    Elf ->
                                        'E'

                                    Goblin ->
                                        'G'
                    )
    in
    List.range 0 30
        |> List.map
            (\y ->
                List.range 0 30
                    |> List.map
                        (\x ->
                            case Dict.get ( y, x ) mappedBoard of
                                Nothing ->
                                    ' '

                                Just c ->
                                    c
                        )
                    |> String.fromList
            )
        |> String.join "\n"


debugBoard : State -> State
debugBoard state =
    let
        s =
            drawBoard state

        _ =
            Debug.log s ( state.heroes, state.roundCount )
    in
    state


parseInput : String -> State
parseInput input =
    input
        |> String.lines
        |> List.foldl (\line ( state, y ) -> ( parseLine line y state, y + 1 )) ( State Dict.empty Dict.empty 0, 0 )
        |> Tuple.first



-- test1 =
--     parseInput inputtestmove |> debugBoard |> step |> debugBoard |> step |> debugBoard |> step |> debugBoard
-- test2 =
--     parseInput inputt2 |> stepUntilEnd |> debugBoard |> calculateOutcome |> Debug.log "roundCount"
-- try1 =
--     parseInput input |> stepUntilEnd |> debugBoard |> calculateOutcome |> Debug.log "roundCount"


stepUntilEnd : State -> State
stepUntilEnd state =
    if state.heroes |> Dict.filter (\_ v -> v.hero == Elf) |> Dict.isEmpty then
        state
    else if state.heroes |> Dict.filter (\_ v -> v.hero == Goblin) |> Dict.isEmpty then
        state
    else
        stepUntilEnd (step (always 3) state)


calculateOutcome : State -> Int
calculateOutcome state =
    state.heroes |> Dict.foldl (\_ v i -> i + v.health) 0 |> (*) (state.roundCount - 2)



-- stepXTimes : Int -> State -> State
-- stepXTimes i state =
--     if state.roundCount == i then
--         state
--     else
--         stepXTimes i (step state)


stepUntilElfDie : (Hero -> Int) -> State -> ( Bool, State )
stepUntilElfDie hit state =
    let
        newState =
            step hit state

        elves s =
            s |> Dict.filter (\_ v -> v.hero == Elf)
    in
    if Dict.size (elves state.heroes) > Dict.size (elves newState.heroes) then
        ( False, newState )
    else if state.heroes |> Dict.filter (\_ v -> v.hero == Goblin) |> Dict.isEmpty then
        ( True, newState )
    else
        stepUntilElfDie hit newState



-- findElfPower :


try2 =
    parseInput input
        |> stepUntilElfDie
            (\hero ->
                case hero of
                    Goblin ->
                        3

                    Elf ->
                        14
            )
        |> Tuple.mapSecond (debugBoard >> calculateOutcome)
        |> Debug.log "a"


inputtest : String
inputtest =
    """#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"""


inputtestmove : String
inputtestmove =
    """#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########"""


inputt2 =
    """#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"""


input : String
input =
    """################################
#######..##########.##.G.##.####
#######...#######........#..####
#######..G.######..#...##G..####
########..G###........G##...####
######....G###....G....###.#####
######....####..........##..####
#######...###...........##..E..#
#######.G..##...........#.#...##
######....#.#.....#..GG......###
#####..#..G...G........G.#....##
##########.G.......G........####
#########.G.G.#####EE..E...#####
#########....#######.......#####
#########...#########.......####
########....#########...G...####
#########...#########.#....#####
##########..#########.#E...E####
######....#.#########........#.#
######..G.#..#######...........#
#####.........#####.E......#####
####........................####
####.........G...####.....######
##................##......######
##..........##.##.........######
#............########....E######
####..........#######.E...######
####........#..######...########
########....#.E#######....######
#########...####################
########....####################
################################"""
