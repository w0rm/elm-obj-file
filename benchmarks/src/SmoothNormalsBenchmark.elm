module SmoothNormalsBenchmark exposing (main)

{-| Compares Dict vs Array as the backing store for SmoothNormals.

The Dict is keyed by position index; the Array is indexed directly by position.
The question is whether the O(1) array access beats the O(log n) dict access
enough to justify pre-allocating an array sized to *all* positions, even when
smooth groups only cover a fraction of them.

Two scenarios are tested:
  - dense:  all 1 000 positions are in a smooth group
  - sparse: only 200 out of 5 000 positions are in a smooth group

Each scenario measures a full build-then-lookup cycle.

-}

import Array exposing (Array)
import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict exposing (Dict)
import Random


type alias Normal =
    ( Float, Float, Float )



-- MAIN


main : BenchmarkProgram
main =
    program <|
        Benchmark.describe "SmoothNormals: Dict vs Array"
            [ Benchmark.compare "dense (100 positions, all smooth)"
                "Dict"
                (\_ -> buildAndQueryDict Dict.empty denseInserts denseQueries)
                "Array"
                (\_ -> buildAndQueryArray 100 denseInserts denseQueries)
            , Benchmark.compare "sparse (500 positions, 4% smooth)"
                "Dict"
                (\_ -> buildAndQueryDict Dict.empty sparseInserts sparseQueries)
                "Array"
                (\_ -> buildAndQueryArray 500 sparseInserts sparseQueries)
            ]



-- BUILD + QUERY


{-| Build a Dict from entries, then sum up hits across queries (to prevent
the compiler from optimizing the lookups away).
-}
buildAndQueryDict : Dict Int (List ( Int, Normal )) -> List ( Int, Int, Normal ) -> List ( Int, Int ) -> Int
buildAndQueryDict empty inserts queries =
    let
        dict =
            List.foldl insertDict empty inserts
    in
    List.foldl (\( p, sg ) acc -> acc + lookupDict p sg dict) 0 queries


{-| Initialize an Array of the given size, build from entries, then query.
The Array.repeat is included because it is part of the real cost that Dict
does not pay.
-}
buildAndQueryArray : Int -> List ( Int, Int, Normal ) -> List ( Int, Int ) -> Int
buildAndQueryArray size inserts queries =
    let
        arr =
            List.foldl insertArray (Array.repeat size []) inserts
    in
    List.foldl (\( p, sg ) acc -> acc + lookupArray p sg arr) 0 queries



-- DICT OPERATIONS


insertDict : ( Int, Int, Normal ) -> Dict Int (List ( Int, Normal )) -> Dict Int (List ( Int, Normal ))
insertDict ( p, sg, normal ) dict =
    Dict.insert p
        (addNormal sg normal (Maybe.withDefault [] (Dict.get p dict)) [])
        dict


lookupDict : Int -> Int -> Dict Int (List ( Int, Normal )) -> Int
lookupDict p sg dict =
    case Dict.get p dict of
        Nothing ->
            0

        Just entries ->
            if memberEntry sg entries then
                1

            else
                0



-- ARRAY OPERATIONS


insertArray : ( Int, Int, Normal ) -> Array (List ( Int, Normal )) -> Array (List ( Int, Normal ))
insertArray ( p, sg, normal ) arr =
    Array.set p
        (addNormal sg normal (Maybe.withDefault [] (Array.get p arr)) [])
        arr


lookupArray : Int -> Int -> Array (List ( Int, Normal )) -> Int
lookupArray p sg arr =
    case Array.get p arr of
        Nothing ->
            0

        Just entries ->
            if memberEntry sg entries then
                1

            else
                0



-- SHARED HELPERS


{-| Mirrors SmoothNormals.addNormal: merge into existing entry for the same
smoothing group, or prepend a new one.
-}
addNormal : Int -> Normal -> List ( Int, Normal ) -> List ( Int, Normal ) -> List ( Int, Normal )
addNormal sg ( nx, ny, nz ) entries out =
    case entries of
        [] ->
            ( sg, ( nx, ny, nz ) ) :: out

        (( currentSg, ( cx, cy, cz ) ) as entry) :: rest ->
            if sg == currentSg then
                ( sg, ( nx + cx, ny + cy, nz + cz ) ) :: List.append rest out

            else
                addNormal sg ( nx, ny, nz ) rest (entry :: out)


memberEntry : Int -> List ( Int, Normal ) -> Bool
memberEntry sg entries =
    case entries of
        [] ->
            False

        ( currentSg, _ ) :: rest ->
            if sg == currentSg then
                True

            else
                memberEntry sg rest



-- DATA


normalGenerator : Random.Generator Normal
normalGenerator =
    Random.map3 (\x y z -> ( x, y, z ))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)


{-| Dense: 300 inserts across 100 positions (avg 3 faces per vertex).
-}
denseInserts : List ( Int, Int, Normal )
denseInserts =
    Random.step
        (Random.list 300
            (Random.map3 (\p sg n -> ( p, sg, n ))
                (Random.int 0 99)
                (Random.constant 1)
                normalGenerator
            )
        )
        (Random.initialSeed 42)
        |> Tuple.first


{-| Sparse: 300 inserts but only positions 0–19 are used (out of 500 total).
-}
sparseInserts : List ( Int, Int, Normal )
sparseInserts =
    Random.step
        (Random.list 300
            (Random.map3 (\p sg n -> ( p, sg, n ))
                (Random.int 0 19)
                (Random.constant 1)
                normalGenerator
            )
        )
        (Random.initialSeed 43)
        |> Tuple.first


{-| 50 queries across the dense range.
-}
denseQueries : List ( Int, Int )
denseQueries =
    Random.step
        (Random.list 50
            (Random.map2 Tuple.pair
                (Random.int 0 99)
                (Random.constant 1)
            )
        )
        (Random.initialSeed 100)
        |> Tuple.first


{-| 50 queries across the full sparse range (many will be misses).
-}
sparseQueries : List ( Int, Int )
sparseQueries =
    Random.step
        (Random.list 50
            (Random.map2 Tuple.pair
                (Random.int 0 499)
                (Random.constant 1)
            )
        )
        (Random.initialSeed 101)
        |> Tuple.first
