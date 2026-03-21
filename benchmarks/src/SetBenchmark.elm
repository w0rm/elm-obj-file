module SetBenchmark exposing (main)

import Benchmark
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Random
import Set exposing (Set)


main : BenchmarkProgram
main =
    program <|
        Benchmark.compare "Set construction"
            "build list then fromList"
            (\_ -> buildList elements [])
            "insert one by one"
            (\_ -> insertAll elements Set.empty)


buildList : List Int -> List Int -> Set Int
buildList list acc =
    case list of
        [] ->
            Set.fromList acc

        x :: rest ->
            buildList rest (x :: acc)


insertAll : List Int -> Set Int -> Set Int
insertAll list acc =
    case list of
        [] ->
            acc

        x :: rest ->
            insertAll rest (Set.insert x acc)


elements : List Int
elements =
    Random.step (Random.list 100 (Random.int 0 1000)) (Random.initialSeed 42)
        |> Tuple.first
