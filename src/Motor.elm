module Motor exposing
    ( Motor
    , add
    , clean
    , decoder
    , default
    , encoder
    , getAllLinks
    , getMotored
    , remove
    )

import Coll exposing (Coll, Id)
import Json.Decode as D
import Json.Encode as E
import Link exposing (DrawLink, Link)



-- TODO use update, the point is that update should be the only exposed function to modify model
-- Do the same choice everywhere
-- TODO check that Coll.idMap is the boundary, entering and exiting, Id Motor is intern and Id Motored g is extern
-- Do the same for Harmony and Wheel


type alias Motor =
    List MotorId


type alias Motored g =
    { g | motor : Motor }


type MotorId
    = MID (Id Motor)


default : Motor
default =
    []


toIdList : Motored g -> List (Id (Motored g))
toIdList { motor } =
    List.map (\(MID id) -> Coll.idMap id) motor


addLink : Link (Motored g) -> Coll (Motored g) -> Coll (Motored g)
addLink l coll =
    let
        addLinkId : Id (Motored g) -> Motored g -> Motored g
        addLinkId id g =
            { g | motor = (MID <| Coll.idMap id) :: g.motor }
    in
    coll
        |> (Coll.update (Tuple.first l) <| addLinkId <| Tuple.second l)
        |> (Coll.update (Tuple.second l) <| addLinkId <| Tuple.first l)


rmLink : Link (Motored g) -> Coll (Motored g) -> Coll (Motored g)
rmLink l coll =
    let
        rmLinkId : Id (Motored g) -> Motored g -> Motored g
        rmLinkId id g =
            { g | motor = List.filter ((/=) <| MID <| Coll.idMap id) g.motor }
    in
    coll
        |> (Coll.update (Tuple.first l) <| rmLinkId <| Tuple.second l)
        |> (Coll.update (Tuple.second l) <| rmLinkId <| Tuple.first l)


add : Link (Motored g) -> Coll (Motored g) -> List (Id (Motored g)) -> ( Coll (Motored g), List (Id (Motored g)) )
add l motors playing =
    let
        addPlaying =
            case ( List.member (Tuple.first l) playing, List.member (Tuple.second l) playing ) of
                ( True, False ) ->
                    visitMotors motors (Tuple.second l) []

                ( False, True ) ->
                    visitMotors motors (Tuple.first l) []

                _ ->
                    []
    in
    ( addLink l motors, addPlaying )


remove : List (Link (Motored g)) -> Id (Motored g) -> Coll (Motored g) -> Bool -> ( Coll (Motored g), List (Id (Motored g)) )
remove ls entry motors playing =
    let
        newMotors =
            List.foldl rmLink motors ls
    in
    ( newMotors
    , if playing then
        visitMotors newMotors entry []

      else
        []
    )


clean : Id (Motored g) -> Coll (Motored g) -> Coll (Motored g)
clean id motors =
    List.foldl rmLink motors <|
        List.map (\(MID otherId) -> ( id, Coll.idMap otherId )) <|
            .motor <|
                Coll.get id motors


getMotored : Id (Motored g) -> Coll (Motored g) -> List (Id (Motored g))
getMotored entry motors =
    visitMotors motors entry []


getAllLinks : Coll (Motored g) -> List (Link (Motored g))
getAllLinks motors =
    Coll.ids motors
        |> List.foldl (\id -> visitToLinks motors id Nothing) ( [], [] )
        |> Tuple.second


encoder : Motor -> E.Value
encoder l =
    E.list Coll.idEncoder <| List.map (\(MID id) -> id) l


decoder : D.Decoder Motor
decoder =
    D.list <| D.map MID Coll.idDecoder



-- TODO sometimes double Link in the list
-- either Set Link or switch from adjacency list to edge list ?


visitToLinks :
    Coll (Motored g)
    -> Id (Motored g)
    -> Maybe (Id (Motored g))
    -> ( List (Id (Motored g)), List (Link (Motored g)) )
    -> ( List (Id (Motored g)), List (Link (Motored g)) )
visitToLinks motors entry mayFromId ( visited, links ) =
    if List.member entry visited then
        ( visited, links )

    else
        (toIdList <| Coll.get entry motors)
            |> List.foldl
                (\neighbour ( v, l ) ->
                    if Just neighbour == mayFromId then
                        ( v, l )

                    else
                        visitToLinks motors neighbour (Just entry) ( v, ( entry, neighbour ) :: l )
                )
                ( entry :: visited, links )


visitMotors : Coll (Motored g) -> Id (Motored g) -> List (Id (Motored g)) -> List (Id (Motored g))
visitMotors motors entry visited =
    if List.member entry visited then
        visited

    else
        (toIdList <| Coll.get entry motors)
            |> List.foldl
                (\neighbour -> visitMotors motors neighbour)
                (entry :: visited)
