module DictUpdateBenchmark exposing (main)

import Benchmark
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict exposing (Dict)
import Random


main : BenchmarkProgram
main =
    program <|
        Benchmark.compare "Dict accumulate"
            "Dict.get + case + Dict.insert"
            (\_ -> accumGetInsert entries Dict.empty)
            "Dict.update with lambda"
            (\_ -> accumUpdate entries Dict.empty)


accumGetInsert : List ( Int, Int ) -> Dict Int Int -> Dict Int Int
accumGetInsert list acc =
    case list of
        [] ->
            acc

        ( k, v ) :: rest ->
            case Dict.get k acc of
                Just prev ->
                    accumGetInsert rest (Dict.insert k (prev + v) acc)

                Nothing ->
                    accumGetInsert rest (Dict.insert k v acc)


accumUpdate : List ( Int, Int ) -> Dict Int Int -> Dict Int Int
accumUpdate list acc =
    case list of
        [] ->
            acc

        ( k, v ) :: rest ->
            accumUpdate rest
                (Dict.update k
                    (\existing ->
                        case existing of
                            Just prev ->
                                Just (prev + v)

                            Nothing ->
                                Just v
                    )
                    acc
                )


entries : List ( Int, Int )
entries =
    Random.step (Random.list 100 (Random.pair (Random.int 0 50) (Random.int 1 100))) (Random.initialSeed 42)
        |> Tuple.first
