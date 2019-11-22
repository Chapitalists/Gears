module Gear exposing (..)

import Coll exposing (Coll, Id)
import Color
import Fraction as Fract exposing (Fraction)
import Html.Attributes
import Interact
import Json.Encode as E
import Math.Vector2 exposing (..)
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (Length(..), Transform(..))



--TODO Could be a type alias ?


type Gear
    = G
        { ref : Ref
        , fract : Fraction

        -- TODO better be a Set than a List, either deOpacify Id or add Set in Coll lib
        , motors : List (Id Gear)
        , pos : Vec2
        , startPercent : Float
        , sound : Sound
        , mute : Bool
        }


getMotors : Gear -> List (Id Gear)
getMotors (G g) =
    g.motors


addMotorLink : Link -> Coll Gear -> Coll Gear
addMotorLink l coll =
    let
        getGear id =
            Coll.get id coll

        addMotor id (G g) =
            G { g | motors = id :: g.motors }
    in
    case Tuple.mapBoth getGear getGear l of
        ( Just _, Just _ ) ->
            coll
                |> Coll.update (Tuple.first l) (addMotor <| Tuple.second l)
                |> Coll.update (Tuple.second l) (addMotor <| Tuple.first l)

        _ ->
            debugGear (Tuple.first l) "Didn’t found gears to add Motor" coll


rmMotorLink : Link -> Coll Gear -> Coll Gear
rmMotorLink l coll =
    let
        getGear id =
            Coll.get id coll

        rmMotor id (G g) =
            G { g | motors = List.filter (\el -> el /= id) g.motors }
    in
    case Tuple.mapBoth getGear getGear l of
        ( Just _, Just _ ) ->
            coll
                |> Coll.update (Tuple.first l) (rmMotor <| Tuple.second l)
                |> Coll.update (Tuple.second l) (rmMotor <| Tuple.first l)

        _ ->
            debugGear (Tuple.first l) "Didn’t found gears to rm Motor" coll


type Ref
    = Other (Id Gear)
    | Self
        { unit : Float

        -- TODO better be a Set than a List, either deOpacify Id or add Set in Coll lib
        , group : List (Id Gear)
        , links : List Link
        }


type alias Link =
    ( Id Gear, Id Gear )


newSelfRef length =
    Self { unit = length, group = [], links = [] }


hasHarmonics : Gear -> Bool
hasHarmonics (G g) =
    case g.ref of
        Self { group } ->
            if List.isEmpty group then
                False

            else
                True

        Other _ ->
            False


getBaseId : Gear -> Maybe (Id Gear)
getBaseId (G g) =
    case g.ref of
        Self _ ->
            Nothing

        Other id ->
            Just id


addToRefGroup : Id Gear -> Gear -> Gear
addToRefGroup id (G g) =
    case g.ref of
        Other _ ->
            debugGear id "Can’t add to ref group if not base" (G g)

        Self r ->
            G { g | ref = Self { r | group = id :: r.group } }


removeFromRefGroup : Id Gear -> Gear -> Gear
removeFromRefGroup id (G g) =
    case g.ref of
        Other _ ->
            debugGear id "Can’t remove from ref group if not base" (G g)

        Self r ->
            let
                isGoodLink l =
                    Tuple.first l /= id && Tuple.second l /= id
            in
            G
                { g
                    | ref =
                        Self
                            { r
                                | group = List.filter ((/=) id) r.group
                                , links = List.filter isGoodLink r.links
                            }
                }


addLink : Link -> Gear -> Gear
addLink l (G g) =
    case g.ref of
        Other _ ->
            Debug.log "Can’t add link if not base" (G g)

        Self r ->
            G { g | ref = Self { r | links = l :: r.links } }


getGearLinks : Gear -> List Link
getGearLinks (G g) =
    case g.ref of
        Other _ ->
            []

        Self { links } ->
            links


stringType : String
stringType =
    "gear"


toUID : Id Gear -> String
toUID id =
    stringType ++ "-" ++ Coll.idToString id


fromSound : Sound -> Vec2 -> Gear
fromSound s p =
    G
        { ref = newSelfRef <| Sound.length s
        , fract = Fract.integer 1
        , motors = []
        , pos = p
        , startPercent = 0
        , sound = s
        , mute = False
        }


getMute : Gear -> Bool
getMute (G g) =
    g.mute


setMute : Bool -> Gear -> Gear
setMute mute (G g) =
    G { g | mute = mute }


copy : Id Gear -> Coll Gear -> Coll Gear
copy id coll =
    case Coll.get id coll of
        Nothing ->
            debugGear id "No gear to copy" coll

        Just (G g) ->
            let
                newG =
                    G
                        { g
                            | pos = add g.pos (vec2 (getLength (G g) coll * 1.1) 0)
                            , ref = Other id
                        }

                ( newId, newColl ) =
                    Coll.insertTellId newG coll
            in
            Coll.update id (addToRefGroup newId >> addLink ( id, newId )) newColl


resizeFree : Id Gear -> Float -> Coll Gear -> Coll Gear
resizeFree id length coll =
    case Coll.get id coll of
        Nothing ->
            debugGear id "No gear to resize" coll

        Just (G g) ->
            case g.ref of
                Self r ->
                    Coll.update id
                        (\(G gg) -> G { gg | ref = Self { r | unit = length / Fract.toFloat g.fract } })
                        coll

                Other rId ->
                    coll
                        |> Coll.update id (\(G gg) -> G { gg | ref = newSelfRef length, fract = Fract.unit 1 })
                        |> Coll.update rId (removeFromRefGroup id)


getFract : Gear -> Fraction
getFract (G g) =
    g.fract


getLength : Gear -> Coll Gear -> Float
getLength (G g) coll =
    case g.ref of
        Self { unit } ->
            unit * Fract.toFloat g.fract

        Other id ->
            case Coll.get id coll of
                Just (G { ref }) ->
                    case ref of
                        Self { unit } ->
                            unit * Fract.toFloat g.fract

                        Other _ ->
                            Debug.log "IMPOSSIBLE Ref isn’t a base" 0

                Nothing ->
                    Debug.log "IMPOSSIBLE Ref doesn’t exist" 0


getPos : Gear -> Vec2
getPos (G g) =
    g.pos


encoder : Id Gear -> Coll Gear -> E.Value
encoder id coll =
    case Coll.get id coll of
        Nothing ->
            debugGear id "No gear to encode" E.null

        Just (G g) ->
            let
                length =
                    getLength (G g) coll
            in
            if length == 0 then
                debugGear id "Length is 0" E.null

            else
                E.object
                    [ ( "id", E.string <| toUID id )
                    , ( "length", E.float length )
                    , ( "soundName", E.string <| Sound.toString g.sound )
                    , ( "mute", E.bool g.mute )
                    ]


type Mod
    = None
    | Hovered
    | Clicked
    | Dragged
    | Resizable


type Msg
    = Move Vec2
    | ResizeFract Fraction


update : Msg -> Gear -> Gear
update msg (G g) =
    case msg of
        Move d ->
            G { g | pos = add d g.pos }

        ResizeFract f ->
            G { g | fract = Fract.multiplication g.fract f }


type OutMsg
    = InteractMsg (Interact.Msg String)
    | GearMsg ( Id Gear, Msg )


view : ( Id Gear, Gear ) -> Coll Gear -> Mod -> Svg OutMsg
view ( id, G g ) coll mod =
    let
        length =
            getLength (G g) coll

        tickH =
            length / 15

        tickW =
            length / 30
    in
    S.g
        ([ SA.transform [ Translate (getX g.pos) (getY g.pos) ] ]
            ++ (List.map (Html.Attributes.map InteractMsg) <|
                    Interact.hoverEvents (toUID id)
               )
        )
        ([ S.g [ Html.Attributes.id <| toUID id ]
            [ S.circle
                ([ SA.cx <| Num 0
                 , SA.cy <| Num 0
                 , SA.r <| Num (length / 2)
                 ]
                    ++ (List.map (Html.Attributes.map InteractMsg) <|
                            Interact.draggableEvents (toUID id)
                       )
                )
                []
            , S.rect
                [ SA.width <| Num tickW
                , SA.height <| Num tickH
                , SA.x <| Num (tickW / -2)
                , SA.y <| Num ((length / -2) - tickH)
                ]
                []
            , S.rect
                [ SA.width <| Num tickW
                , SA.height <| Num tickH
                , SA.x <| Num (tickW / -2)
                , SA.y <| Num (tickH / -2)
                , SA.fill <| TypedSvg.Types.Fill Color.orange
                , SA.transform [ Rotate (g.startPercent * 360) 0 0, Translate 0 ((length / -2) + (tickH / 2)) ]
                ]
                []
            ]
         ]
            ++ (if mod == Resizable then
                    [ S.polyline
                        [ SA.points [ ( -length / 2, 0 ), ( length / 2, 0 ) ]
                        , SA.stroke Color.red
                        , SA.strokeWidth <| Num tickW
                        ]
                        []
                    , S.circle
                        ([ SA.cx <| Num (-length / 2)
                         , SA.cy <| Num 0
                         , SA.r <| Num (tickW * 2)
                         ]
                            ++ (List.map (Html.Attributes.map InteractMsg) <|
                                    Interact.draggableEvents ("resize.left." ++ toUID id)
                               )
                        )
                        []
                    , S.circle
                        ([ SA.cx <| Num (length / 2)
                         , SA.cy <| Num 0
                         , SA.r <| Num (tickW * 2)
                         ]
                            ++ (List.map (Html.Attributes.map InteractMsg) <|
                                    Interact.draggableEvents ("resize.right." ++ toUID id)
                               )
                        )
                        []
                    ]

                else
                    []
               )
        )


debugGear : Id Gear -> String -> default -> default
debugGear id str =
    Debug.log (str ++ " " ++ toUID id)
