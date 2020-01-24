module Coll exposing
    ( Coll
    , Id
    , decoder
    , empty
    , encoder
    , filter
    , get
    , idDecoder
    , idEncoder
    , idMap
    , idToString
    , ids
    , insert
    , insertTellId
    , isEmpty
    , maybeGet
    , remove
    , startId
    , toList
    , update
    , values
    )

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E


type Id x
    = Id Int


getIdInternal : Id x -> Int
getIdInternal (Id i) =
    i


idToString : Id x -> String
idToString (Id i) =
    String.fromInt i


idEncoder : Id x -> E.Value
idEncoder (Id i) =
    E.int i


idDecoder : D.Decoder (Id x)
idDecoder =
    D.map Id D.int


idMap : Id a -> Id b
idMap (Id i) =
    Id i


type Coll item
    = C
        { nextId : Int
        , d : Dict Int item
        , default : item
        , typeString : String
        }


startInt : Int
startInt =
    1


startId : Id item
startId =
    Id startInt


empty : String -> item -> Coll item
empty typeString default =
    C { nextId = startInt, d = Dict.empty, default = default, typeString = typeString }


isEmpty : Coll item -> Bool
isEmpty (C { d }) =
    Dict.isEmpty d


get : Id item -> Coll item -> item
get id (C coll) =
    case maybeGet id (C coll) of
        Nothing ->
            Debug.log ("No " ++ coll.typeString ++ " for id " ++ (String.fromInt <| getIdInternal id)) coll.default

        Just item ->
            item


maybeGet : Id item -> Coll item -> Maybe item
maybeGet id (C { d }) =
    Dict.get (getIdInternal id) d


insert : item -> Coll item -> Coll item
insert el (C coll) =
    C { coll | d = Dict.insert coll.nextId el coll.d, nextId = coll.nextId + 1 }


insertTellId : item -> Coll item -> ( Id item, Coll item )
insertTellId el (C coll) =
    ( Id coll.nextId, C { coll | d = Dict.insert coll.nextId el coll.d, nextId = coll.nextId + 1 } )


remove : Id item -> Coll item -> Coll item
remove id (C coll) =
    C { coll | d = Dict.remove (getIdInternal id) coll.d }


update : Id item -> (item -> item) -> Coll item -> Coll item
update id f (C coll) =
    C { coll | d = Dict.update (getIdInternal id) (Maybe.map f) coll.d }


filter : (item -> Bool) -> Coll item -> Coll item
filter isGood (C coll) =
    C { coll | d = Dict.filter (\_ v -> isGood v) coll.d }


toList : Coll item -> List ( Id item, item )
toList (C { d }) =
    Dict.toList d
        |> List.map (Tuple.mapFirst Id)


ids : Coll item -> List (Id item)
ids (C { d }) =
    List.map Id <| Dict.keys d


values : Coll item -> List item
values (C { d }) =
    Dict.values d


encoder : Coll item -> (item -> E.Value) -> E.Value
encoder (C coll) itemEncoder =
    E.list (keyValueEncoder itemEncoder) (Dict.toList coll.d)


decoder : D.Decoder item -> String -> item -> D.Decoder (Coll item)
decoder itemDecoder typeString defaultItem =
    D.map (fromKeyValue typeString defaultItem) <| D.list (keyValueDecoder itemDecoder)


keyValueEncoder : (item -> E.Value) -> ( Int, item ) -> E.Value
keyValueEncoder itemEncoder ( i, item ) =
    E.object [ ( "id", E.int i ), ( "item", itemEncoder item ) ]


keyValueDecoder : D.Decoder item -> D.Decoder ( Int, item )
keyValueDecoder itemDecoder =
    Field.require "id" D.int <|
        \i ->
            Field.require "item" itemDecoder <|
                \item ->
                    D.succeed ( i, item )


fromKeyValue : String -> item -> List ( Int, item ) -> Coll item
fromKeyValue typeString default l =
    let
        ( ints, _ ) =
            List.unzip l
    in
    C
        { d = Dict.fromList l
        , default = default
        , nextId =
            case List.maximum ints of
                Nothing ->
                    getIdInternal startId

                Just i ->
                    i + 1
        , typeString = typeString
        }
