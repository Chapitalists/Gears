module Gear exposing (..)

import Coll exposing (Coll, Id)
import Color
import Fraction as Fract exposing (Fraction)
import Html.Attributes
import Interact
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Math.Vector2 exposing (..)
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (Fill(..), Length(..), Opacity(..), Transform(..))


type alias Gear =
    { ref : Ref
    , fract : Fraction

    -- TODO better be a Set than a List, either deOpacify Id or add Set in Coll lib
    , motors : List GearId
    , pos : Vec2
    , startPercent : Float
    , volume : Float
    , sound : Sound
    , mute : Bool
    }



-- To prevent recursive type


type GearId
    = G (Id Gear)


default =
    fromSound Sound.noSound <| vec2 0 0


getMotors : Gear -> List (Id Gear)
getMotors g =
    List.map (\(G id) -> id) g.motors


addMotorLink : Link -> Coll Gear -> Coll Gear
addMotorLink l coll =
    let
        addMotor id g =
            { g | motors = G id :: g.motors }
    in
    coll
        |> Coll.update (Tuple.first l) (addMotor <| Tuple.second l)
        |> Coll.update (Tuple.second l) (addMotor <| Tuple.first l)


rmMotorLink : Link -> Coll Gear -> Coll Gear
rmMotorLink l coll =
    let
        rmMotor id g =
            { g | motors = List.filter (\el -> el /= G id) g.motors }
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
hasHarmonics g =
    case g.ref of
        Self { group } ->
            if List.isEmpty group then
                False

            else
                True

        Other _ ->
            False


getHarmonicGroup : Id Gear -> Coll Gear -> List (Id Gear)
getHarmonicGroup id coll =
    let
        g =
            Coll.get id coll
    in
    case g.ref of
        Self { group } ->
            id :: group

        Other _ ->
            [ id ]


getBaseId : Gear -> Maybe (Id Gear)
getBaseId g =
    case g.ref of
        Self _ ->
            Nothing

        Other id ->
            Just id


addToRefGroup : Id Gear -> Gear -> Gear
addToRefGroup id g =
    case g.ref of
        Other _ ->
            debugGear id "Can’t add to ref group if not base" g

        Self r ->
            { g | ref = Self { r | group = id :: r.group } }


removeFromRefGroup : Id Gear -> Gear -> Gear
removeFromRefGroup id g =
    case g.ref of
        Other _ ->
            debugGear id "Can’t remove from ref group if not base" g

        Self r ->
            let
                isGoodLink l =
                    Tuple.first l /= id && Tuple.second l /= id
            in
            { g
                | ref =
                    Self
                        { r
                            | group = List.filter ((/=) id) r.group
                            , links = List.filter isGoodLink r.links
                        }
            }


addLink : Link -> Gear -> Gear
addLink l g =
    case g.ref of
        Other _ ->
            Debug.log "Can’t add link if not base" g

        Self r ->
            { g | ref = Self { r | links = l :: r.links } }


isActiveLink : Link -> Gear -> Bool
isActiveLink l g =
    case g.ref of
        Other _ ->
            Debug.log "Can’t check active links if not base" False

        Self { links } ->
            List.any (equalLinks l) links


getGearLinks : Gear -> List Link
getGearLinks g =
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
    { ref = newSelfRef <| Sound.length s
    , fract = Fract.integer 1
    , motors = []
    , pos = p
    , startPercent = 0
    , volume = 1
    , sound = s
    , mute = False
    }


getMute : Gear -> Bool
getMute g =
    g.mute


setMute : Bool -> Gear -> Gear
setMute mute g =
    { g | mute = mute }


copy : Id Gear -> Coll Gear -> Coll Gear
copy id coll =
    let
        g =
            Coll.get id coll

        base =
            Maybe.withDefault id <| getBaseId g

        newG =
            { g
                | pos = add g.pos (vec2 (getLength g coll * 1.1) 0)
                , ref = Other base
                , motors = []
            }

        ( newId, newColl ) =
            Coll.insertTellId newG coll
    in
    Coll.update base (addToRefGroup newId >> addLink ( id, newId )) newColl


resizeFree : Id Gear -> Float -> Coll Gear -> Coll Gear
resizeFree id length coll =
    let
        g =
            Coll.get id coll
    in
    case g.ref of
        Self r ->
            Coll.update id
                (\gg -> { gg | ref = Self { r | unit = length / Fract.toFloat g.fract } })
                coll

        Other rId ->
            coll
                |> Coll.update id (\gg -> { gg | ref = newSelfRef length, fract = Fract.unit 1 })
                |> Coll.update rId (removeFromRefGroup id)


getFract : Gear -> Fraction
getFract g =
    g.fract


setFract : Fraction -> Gear -> Gear
setFract f g =
    { g | fract = f }


getVolume : Gear -> Float
getVolume g =
    g.volume


getLengthId : Id Gear -> Coll Gear -> Float
getLengthId id coll =
    getLength (Coll.get id coll) coll


getLength : Gear -> Coll Gear -> Float
getLength g coll =
    case g.ref of
        Self { unit } ->
            unit * Fract.toFloat g.fract

        Other id ->
            let
                { ref } =
                    Coll.get id coll
            in
            case ref of
                Self { unit } ->
                    unit * Fract.toFloat g.fract

                Other _ ->
                    Debug.log "IMPOSSIBLE Ref isn’t a base" 0


getPos : Gear -> Vec2
getPos g =
    g.pos


encoderToEngine : Id Gear -> Coll Gear -> E.Value
encoderToEngine id coll =
    let
        g =
            Coll.get id coll

        length =
            getLength g coll
    in
    if length == 0 then
        debugGear id "Length is 0" E.null

    else
        E.object
            [ ( "id", E.string <| toUID id )
            , ( "length", E.float length )
            , ( "soundName", E.string <| Sound.toString g.sound )
            , ( "mute", E.bool g.mute )
            , ( "volume", E.float g.volume )
            ]


encoderToSave : Gear -> E.Value
encoderToSave g =
    E.object <|
        [ ( "ref", refEncoder g.ref )
        , ( "fract", Fract.encoder g.fract )
        , ( "motors", E.list Coll.idEncoder <| getMotors g )
        , ( "pos"
          , E.object
                [ ( "x", E.float <| getX g.pos )
                , ( "y", E.float <| getY g.pos )
                ]
          )
        , ( "startPercent", E.float g.startPercent )
        , ( "volume", E.float g.volume )
        , ( "sound", Sound.encoder g.sound )
        , ( "mute", E.bool g.mute )
        ]


decoder : D.Decoder Gear
decoder =
    Field.require "ref" refDecoder <|
        \ref ->
            Field.require "fract" Fract.decoder <|
                \fract ->
                    Field.require "motors" (D.list <| D.map G Coll.idDecoder) <|
                        \motors ->
                            Field.requireAt [ "pos", "x" ] D.float <|
                                \px ->
                                    Field.requireAt [ "pos", "y" ] D.float <|
                                        \py ->
                                            Field.require "startPercent" D.float <|
                                                \startPercent ->
                                                    Field.require "volume" D.float <|
                                                        \volume ->
                                                            Field.require "sound" Sound.decoder <|
                                                                \sound ->
                                                                    Field.require "mute" D.bool <|
                                                                        \mute ->
                                                                            D.succeed
                                                                                { ref = ref
                                                                                , fract = fract
                                                                                , motors = motors
                                                                                , pos = vec2 px py
                                                                                , startPercent = startPercent
                                                                                , volume = volume
                                                                                , sound = sound
                                                                                , mute = mute
                                                                                }


refEncoder : Ref -> E.Value
refEncoder ref =
    case ref of
        Other id ->
            E.object [ ( "other", Coll.idEncoder id ) ]

        Self r ->
            E.object <|
                [ ( "unit", E.float r.unit )
                , ( "group", E.list Coll.idEncoder r.group )
                , ( "links", E.list linkEncoder r.links )
                ]


refDecoder : D.Decoder Ref
refDecoder =
    Field.attempt "other" Coll.idDecoder <|
        \mayId ->
            case mayId of
                Just id ->
                    D.succeed <| Other id

                Nothing ->
                    Field.require "unit" D.float <|
                        \unit ->
                            Field.require "group" (D.list Coll.idDecoder) <|
                                \group ->
                                    Field.require "links" (D.list linkDecoder) <|
                                        \links ->
                                            D.succeed <|
                                                Self
                                                    { unit = unit
                                                    , group = group
                                                    , links = links
                                                    }


linkEncoder : Link -> E.Value
linkEncoder l =
    E.object
        [ ( "from", Coll.idEncoder <| Tuple.first l )
        , ( "to", Coll.idEncoder <| Tuple.second l )
        ]


linkDecoder : D.Decoder Link
linkDecoder =
    Field.require "from" Coll.idDecoder <|
        \from ->
            Field.require "to" Coll.idDecoder <|
                \to ->
                    D.succeed ( from, to )


type Mod
    = None
    | Selectable
    | Selected
    | Resizing


type Msg
    = Move Vec2
    | ResizeFract Fraction
    | ChangeVolume Float
    | ChangeSound Sound


update : Msg -> Gear -> Gear
update msg g =
    case msg of
        Move d ->
            { g | pos = add d g.pos }

        ResizeFract f ->
            { g | fract = Fract.multiplication g.fract f }

        ChangeVolume v ->
            { g | volume = clamp 0 1 v }

        ChangeSound s ->
            { g | sound = s }


type Interactable
    = IGear (Id Gear)
    | IResizeHandle (Id Gear) Bool -- True = Right


view : ( Id Gear, Gear ) -> Coll Gear -> Mod -> Svg (Interact.Msg Interactable)
view ( id, g ) coll mod =
    let
        length =
            getLength g coll

        tickH =
            length / 15

        tickW =
            length / 30

        circum =
            length * pi
    in
    S.g
        ([ SA.transform [ Translate (getX g.pos) (getY g.pos) ] ]
            ++ Interact.hoverEvents (IGear id)
        )
        ([ S.g [ Html.Attributes.id <| toUID id ]
            [ S.circle
                ([ SA.cx <| Num 0
                 , SA.cy <| Num 0
                 , SA.r <| Num (length / 2)
                 , SA.stroke <|
                    if id == Coll.startId then
                        Color.red

                    else
                        Color.black
                 , SA.strokeWidth <|
                    Num <|
                        if mod == Selectable then
                            tickW * 2

                        else
                            tickW
                 , SA.strokeDasharray <|
                    if hasHarmonics g then
                        String.fromFloat (circum / 40 * 3 / 4)
                            ++ ","
                            ++ String.fromFloat (circum / 40 * 1 / 4)

                    else
                        ""
                 , SA.fill <|
                    if g.mute then
                        Fill Color.white

                    else
                        Fill Color.black
                 , SA.fillOpacity <| Opacity (0.2 + 0.8 * g.volume)
                 ]
                    ++ Interact.draggableEvents (IGear id)
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
                , SA.fill <| Fill Color.orange
                , SA.transform [ Rotate (g.startPercent * 360) 0 0, Translate 0 ((length / -2) + (tickH / 2)) ]
                ]
                []
            ]
         ]
            ++ (if mod == Selected then
                    [ S.circle
                        [ SA.cx <| Num 0
                        , SA.cy <| Num 0
                        , SA.r <| Num (length / 2 + tickW * 2)
                        , SA.strokeWidth <| Num (tickW / 2)
                        , SA.stroke Color.black
                        , SA.fill FillNone
                        ]
                        []
                    ]

                else
                    []
               )
            ++ (if mod == Resizing then
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
                            ++ Interact.draggableEvents (IResizeHandle id False)
                        )
                        []
                    , S.circle
                        ([ SA.cx <| Num (length / 2)
                         , SA.cy <| Num 0
                         , SA.r <| Num (tickW * 2)
                         ]
                            ++ Interact.draggableEvents (IResizeHandle id True)
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
