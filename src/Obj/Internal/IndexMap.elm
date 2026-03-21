module Obj.Internal.IndexMap exposing
    ( Empty
    , IndexMap
    , Key1
    , Key2
    , Key3
    , empty
    , get
    , get2
    , get3
    , init
    , init2
    , init3
    , insert
    , insert2
    , insert3
    )

import Array exposing (Array)


{-| A map from position index to output vertex index, used to deduplicate
vertices during mesh construction.

Keyed primarily by position index (`p`) for O(1) array access — positions
are dense integers in `0..n-1`, making an array the most efficient structure
here. When vertices must also be distinguished by additional attributes (uv
coordinates, normals, smoothing groups), those are stored as extra keys
alongside the result index in the same slot.

Each slot holds a flat list of interleaved keys and result indices. For two
extra keys this looks like `[ k1a, k2a, idx_a, k1b, k2b, idx_b, … ]`. A list
is used because any given position typically appears in only a small number of
faces, so the linear scan is cheaper in practice than a nested `Dict` or
`Array`.

The phantom type `a` encodes how many keys are stored per entry, preventing
accidental mixing of incompatible maps at compile time.

-}
type IndexMap a
    = IndexMap (Array (List Int))


type Empty
    = Empty Never


type Key1
    = Key1 Never


type Key2
    = Key2 Never


type Key3
    = Key3 Never


empty : Int -> IndexMap Empty
empty size =
    IndexMap (Array.repeat size [])


init : IndexMap Empty -> IndexMap Key1
init (IndexMap arr) =
    IndexMap arr


init2 : IndexMap Empty -> IndexMap Key2
init2 (IndexMap arr) =
    IndexMap arr


init3 : IndexMap Empty -> IndexMap Key3
init3 (IndexMap arr) =
    IndexMap arr


get : Int -> IndexMap Key1 -> Int
get p (IndexMap arr) =
    case Array.get p arr of
        Just (idx :: _) ->
            idx

        _ ->
            -1


get2 : Int -> Int -> IndexMap Key2 -> Int
get2 p key1 (IndexMap arr) =
    case Array.get p arr of
        Just list ->
            lookup1 key1 list

        Nothing ->
            -1


get3 : Int -> Int -> Int -> IndexMap Key3 -> Int
get3 p key1 key2 (IndexMap arr) =
    case Array.get p arr of
        Just list ->
            lookup2 key1 key2 list

        Nothing ->
            -1


insert : Int -> Int -> IndexMap Key1 -> IndexMap Key1
insert p index (IndexMap arr) =
    IndexMap (Array.set p [ index ] arr)


insert2 : Int -> Int -> Int -> IndexMap Key2 -> IndexMap Key2
insert2 p key1 index (IndexMap arr) =
    let
        existing =
            case Array.get p arr of
                Just list ->
                    list

                Nothing ->
                    []
    in
    IndexMap (Array.set p (key1 :: index :: existing) arr)


insert3 : Int -> Int -> Int -> Int -> IndexMap Key3 -> IndexMap Key3
insert3 p key1 key2 index (IndexMap arr) =
    let
        existing =
            case Array.get p arr of
                Just list ->
                    list

                Nothing ->
                    []
    in
    IndexMap (Array.set p (key1 :: key2 :: index :: existing) arr)


lookup1 : Int -> List Int -> Int
lookup1 key1 list =
    case list of
        k1 :: result :: rest ->
            if key1 - k1 == 0 then
                result

            else
                lookup1 key1 rest

        _ ->
            -1


lookup2 : Int -> Int -> List Int -> Int
lookup2 key1 key2 list =
    case list of
        k1 :: k2 :: result :: rest ->
            if key1 - k1 == 0 && key2 - k2 == 0 then
                result

            else
                lookup2 key1 key2 rest

        _ ->
            -1
