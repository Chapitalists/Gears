module Coll exposing (Coll, Id, empty, get, idToString, insert, remove, toList, update)

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


update : Id item -> (item -> item) -> Coll item -> Coll item
update id f (C coll) =
    C { coll | d = Dict.update (getIdInternal id) (Maybe.map f) coll.d }


toList : Coll item -> List ( Id item, item )
toList (C { d }) =
    Dict.toList d
        |> List.map (Tuple.mapFirst opacifyId)
