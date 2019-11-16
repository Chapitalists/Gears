module Coll exposing (Coll, Id, empty, fillReserved, filter, forgeId, get, idToString, insert, remove, reserve, toList, update, values)

import Dict exposing (Dict)


type Id x
    = Id Int


getIdInternal : Id x -> Int
getIdInternal (Id i) =
    i


opacifyId : Int -> Id x
opacifyId i =
    Id i


idToString : Id x -> String
idToString (Id i) =
    String.fromInt i


forgeId : String -> Id x
forgeId str =
    case String.toInt str of
        Just i ->
            Id i

        Nothing ->
            Debug.log ("ERROR Tried to forge id with " ++ str) (Id 0)


type Coll item
    = C { nextId : Int, d : Dict Int item }


empty : Coll item
empty =
    C { nextId = 1, d = Dict.empty }


get : Id item -> Coll item -> Maybe item
get id (C coll) =
    Dict.get (getIdInternal id) coll.d


insert : item -> Coll item -> Coll item
insert el (C coll) =
    C { coll | d = Dict.insert coll.nextId el coll.d, nextId = coll.nextId + 1 }


remove : Id item -> Coll item -> Coll item
remove id (C coll) =
    C { coll | d = Dict.remove (getIdInternal id) coll.d }


reserve : Coll item -> ( Coll item, Id item )
reserve (C coll) =
    ( C { coll | nextId = coll.nextId + 1 }, opacifyId coll.nextId )


fillReserved : Id item -> item -> Coll item -> Coll item
fillReserved id el (C coll) =
    C { coll | d = Dict.insert (getIdInternal id) el coll.d }


update : Id item -> (item -> item) -> Coll item -> Coll item
update id f (C coll) =
    C { coll | d = Dict.update (getIdInternal id) (Maybe.map f) coll.d }


filter : (item -> Bool) -> Coll item -> Coll item
filter isGood (C coll) =
    C { coll | d = Dict.filter (\_ v -> isGood v) coll.d }


toList : Coll item -> List ( Id item, item )
toList (C { d }) =
    Dict.toList d
        |> List.map (Tuple.mapFirst opacifyId)


values : Coll item -> List item
values (C { d }) =
    Dict.values d
