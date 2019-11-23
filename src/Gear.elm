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


default =
    G
        { ref = newSelfRef 0
        , fract = Fract.integer 0
        , motors = []
        , pos = vec2 0 0
        , startPercent = 0
        , sound = Sound.noSound
        , mute = False
        }


getMotors : Gear -> List (Id Gear)
getMotors (G g) =
    g.motors


addMotorLink : Link -> Coll Gear -> Coll Gear
addMotorLink l coll =
    let
        addMotor id (G g) =
            G { g | motors = id :: g.motors }
    in
    coll
        |> Coll.update (Tuple.first l) (addMotor <| Tuple.second l)
        |> Coll.update (Tuple.second l) (addMotor <| Tuple.first l)


rmMotorLink : Link -> Coll Gear -> Coll Gear
rmMotorLink l coll =
    let
        rmMotor id (G g) =
            G { g | motors = List.filter (\el -> el /= id) g.motors }
    in
    coll
        |> Coll.update (Tuple.first l) (rmMotor <| Tuple.second l)
        |> Coll.update (Tuple.second l) (rmMotor <| Tuple.first l)


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


isActiveLink : Link -> Gear -> Bool
isActiveLink l (G g) =
    case g.ref of
        Other _ ->
            Debug.log "Can’t check active links if not base" False

        Self { links } ->
            List.any (equalLinks l) links


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
    let
        (G g) =
            Coll.get id coll

        base =
            Maybe.withDefault id <| getBaseId (G g)

        newG =
            G
                { g
                    | pos = add g.pos (vec2 (getLength (G g) coll * 1.1) 0)
                    , ref = Other base
                }

        ( newId, newColl ) =
            Coll.insertTellId newG coll
    in
    Coll.update base (addToRefGroup newId >> addLink ( id, newId )) newColl


resizeFree : Id Gear -> Float -> Coll Gear -> Coll Gear
resizeFree id length coll =
    let
        (G g) =
            Coll.get id coll
    in
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


setFract : Fraction -> Gear -> Gear
setFract f (G g) =
    G { g | fract = f }


getLength : Gear -> Coll Gear -> Float
getLength (G g) coll =
    case g.ref of
        Self { unit } ->
            unit * Fract.toFloat g.fract

        Other id ->
            let
                (G { ref }) =
                    Coll.get id coll
            in
            case ref of
                Self { unit } ->
                    unit * Fract.toFloat g.fract

                Other _ ->
                    Debug.log "IMPOSSIBLE Ref isn’t a base" 0


getPos : Gear -> Vec2
getPos (G g) =
    g.pos


encoder : Id Gear -> Coll Gear -> E.Value
encoder id coll =
    let
        (G g) =
            Coll.get id coll

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



-- TODO copy of Link.equal because Link needs Gear and can’t cycle import
-- Make Harmonies Module which imports on Gear and Link


equalLinks : Link -> Link -> Bool
equalLinks l1 l2 =
    (Tuple.first l1 == Tuple.first l2 && Tuple.second l1 == Tuple.second l2)
        || (Tuple.first l1 == Tuple.second l2 && Tuple.first l2 == Tuple.second l1)
