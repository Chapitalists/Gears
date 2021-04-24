port module Editor.Mobile exposing (..)

import Coll exposing (Coll, Id)
import Color
import Data.Collar as Collar exposing (Beed)
import Data.Common as CommonData exposing (Identifier)
import Data.Content as Content exposing (Content)
import Data.Gear as Gear
import Data.Mobile as Mobile exposing (Geer, Mobeel)
import Data.Wheel as Wheel exposing (Conteet, Wheel)
import Dict exposing (Dict)
import Editor.Interacting exposing (..)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Engine exposing (Engine)
import File.Download as DL
import Fraction as Fract exposing (Fraction)
import Harmony as Harmo
import Html
import Html.Attributes
import Html.Events
import Interact exposing (Interact)
import Json.Decode as D
import Json.Encode as E
import Link exposing (DrawLink, Link, Segment)
import Math.Vector2 as Vec exposing (Vec2, getX, getY, vec2)
import Motor
import Pack exposing (Pack, Packed)
import PanSvg
import Random
import Round
import Sound exposing (Sound)
import Time
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Length(..), Opacity(..), Transform(..))
import Waveform exposing (Waveform)


port toggleRecord : Bool -> Cmd msg


port gotRecord : (D.Value -> msg) -> Sub msg


port requestCutSample : { fromSoundPath : String, newFileName : String, percents : ( Float, Float ) } -> Cmd msg



-- TODO Maybe delegate Coll dependence to Data.Mobile (except Id)


svgId : String
svgId =
    "svg"


charsInFile : List Char
charsInFile =
    [ '_', '-', ' ' ]


blinkOnTime : Float
blinkOnTime =
    800


blinkOffTime : Float
blinkOffTime =
    200


type alias Model =
    { dragging : Dragging
    , tool : Tool
    , mode : Mode
    , edit : List (Id Geer)
    , beadCursor : Int
    , link : Maybe LinkInfo
    , collarMult : ( Int, Bool )
    , collarDiv : ( Int, Bool )
    , newSampleName : String
    , parentUid : String -- TODO Two sources of truth !! same in Engine
    , engine : Engine
    , interact : Interact.State Interactable Zone
    , pack : Pack
    , wave : Waveform
    , svg : PanSvg.Model
    }


defaultAddPos : Vec2
defaultAddPos =
    vec2 50 50



-- TODO Mix Dragging inside Tool to make impossible states impossible ?


type Tool
    = Edit Bool -- Playing
    | Play Bool Bool -- Playing, Recording
    | Harmonize


type Mode
    = Normal
    | Nav
    | ChangeSound (Id Geer)
    | SupprMode
    | Move
    | SelectMotor
    | Alternate
    | Solo
    | Clone


keyCodeToMode : List ( String, Mode )
keyCodeToMode =
    [ ( "KeyD", Move )
    , ( "KeyV", Nav )
    , ( "Delete", SupprMode )
    , ( "Backspace", SupprMode )
    , ( "KeyQ", Alternate )
    , ( "KeyS", Solo )
    , ( "KeyA", Clone )
    ]


keyCodeToShortcut : Model -> Mobeel -> Dict String Msg
keyCodeToShortcut mod mob =
    Dict.map (always WaveMsg) <| Waveform.keyCodeToShortcut <| getWavePoints mod mob


keyCodeToDirection : Dict String Msg
keyCodeToDirection =
    Dict.map (always WaveMsg) Waveform.keyCodeToDirection


type alias LinkInfo =
    { link : Link Geer, fractInput : FractInput }


type FractInput
    = FractionInput Fraction Bool Bool -- non empty numerator then denominator
    | TextInput String


type Dragging
    = NoDrag
    | HalfLink ( Id Geer, Vec2 )
    | CompleteLink (Link Geer)
    | Cut ( Vec2, Vec2 ) (List (Link Geer))
    | WeaveBeads Segment (List (Id Geer))
    | Alterning Identifier (Maybe Identifier) BlinkState
    | VolumeChange
    | SizeChange
    | Moving
    | Packing
    | Waving
    | Packed Vec2 (Id Packed)
    | Content ( Vec2, Float )
    | ChgContent (Id Geer) Dragging


type alias BlinkState =
    ( Bool, Float )


init : Model
init =
    { dragging = NoDrag
    , tool = Play False False
    , mode = Normal
    , edit = []
    , beadCursor = 0
    , link = Nothing
    , collarMult = ( 4, True )
    , collarDiv = ( 4, True )
    , newSampleName = ""
    , parentUid = ""
    , engine = Engine.init
    , interact = Interact.init
    , pack = Pack.init
    , wave = Waveform.init
    , svg = PanSvg.init svgId
    }


changeView : Maybe Mobeel -> String -> Model -> Model
changeView mayMobile parentUid model =
    let
        svg =
            Maybe.withDefault model.svg <|
                Maybe.map (\m -> PanSvg.centerZoom (Mobile.gearPosSize m.motor m.gears) model.svg) mayMobile
    in
    { model
        | edit = []
        , link = Nothing
        , engine = Engine.setParentUid parentUid Engine.init
        , parentUid = parentUid
        , svg = svg
        , pack = Pack.update (Pack.PrepareZoom svg) model.pack
    }


type Msg
    = ChangedTool Tool
    | ChangedMode Mode
      -- TODO EngineMsg ?
    | ToggleEngine
    | PlayGear
    | StopGear
    | ToggleRecord Bool
    | GotRecord (Result D.Error String)
      -- COLLAR EDIT
    | CursorRight
    | CursorLeft
    | NewBead Conteet
    | UnpackBead ( Wheel, Float ) Bool
      --
    | CopyGear Bool (Id Geer)
    | CopyContent Wheel
    | NewGear Vec2 Conteet
    | DeleteWheel Identifier
    | EnteredFract Bool String -- True for Numerator
    | AppliedFract (Link Geer) Fraction
    | EnteredTextFract String
    | ForcedFract (Link Geer) Fraction
    | SimplifyFractView
    | ResizeToContent (Id Geer)
    | Capsuled (List (Id Geer))
    | Collared (Id Geer) Collaring
    | UnCollar (Id Geer)
    | EnteredCollarMult String
    | EnteredCollarDiv String
    | EnteredNewSampleName String
    | CutNewSample
    | Blink
    | InteractMsg (Interact.Msg Interactable Zone)
    | SvgMsg PanSvg.Msg
    | SVGSize (Result D.Error PanSvg.Size)
    | WheelMsgs (List ( Identifier, Wheel.Msg ))
    | GearMsg ( Id Geer, Gear.Msg )
    | PackMsg Pack.Msg
    | WaveMsg Waveform.Msg
    | OutMsg DocMsg
    | NoMsg


type alias Return =
    { model : Model
    , mobile : Mobeel
    , toUndo : ToUndo
    , toEngine : List E.Value
    , outMsg : Maybe DocMsg
    , cmd : Cmd Msg
    }


type DocMsg
    = Inside Identifier
    | UnSolo


type ToUndo
    = Do
    | Group
    | Cancel
    | NOOP


type Collaring
    = Simple
    | Mult Int
    | Div Sound Int


update : Msg -> ( Model, Mobeel ) -> Return
update msg ( model, mobile ) =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = []
            , outMsg = Nothing
            , cmd = Cmd.none
            }
    in
    case msg of
        ChangedTool tool ->
            { return
                | model =
                    { model
                        | tool = tool
                        , dragging =
                            case model.dragging of
                                Cut _ _ ->
                                    NoDrag

                                WeaveBeads _ _ ->
                                    NoDrag

                                _ ->
                                    case tool of
                                        Edit _ ->
                                            NoDrag

                                        _ ->
                                            model.dragging
                        , engine = Engine.init
                    }
                , toEngine = [ Engine.stop ]
            }

        ChangedMode mode ->
            if model.mode == Solo && mode /= Solo then
                { return | model = { model | mode = mode }, toUndo = Cancel, outMsg = Just UnSolo }

            else
                { return | model = { model | mode = mode } }

        ToggleEngine ->
            if Coll.maybeGet mobile.motor mobile.gears == Nothing then
                return

            else
                case model.tool of
                    Play True r ->
                        { return
                            | model = { model | tool = Play False r, engine = Engine.init }
                            , toEngine = [ Engine.stop ]
                        }

                    Play False r ->
                        let
                            ( engine, v ) =
                                Engine.addPlaying
                                    (Motor.getMotored mobile.motor mobile.gears)
                                    mobile.gears
                                    model.engine
                        in
                        { return | model = { model | tool = Play True r, engine = engine }, toEngine = v }

                    _ ->
                        return

        ToggleRecord rec ->
            case model.tool of
                Play on _ ->
                    { return | model = { model | tool = Play on rec }, cmd = toggleRecord rec }

                _ ->
                    return

        GotRecord res ->
            case res of
                Ok url ->
                    { return | cmd = DL.url url }

                Err err ->
                    let
                        _ =
                            Debug.log (D.errorToString err) res
                    in
                    return

        PlayGear ->
            case model.tool of
                Edit _ ->
                    let
                        demutedMobile =
                            model.edit
                                |> List.foldl (\id mob -> CommonData.updateWheel ( id, [] ) (Wheel.Mute False) mob) mobile

                        ( engine, v ) =
                            Engine.addPlaying model.edit demutedMobile.gears model.engine
                    in
                    { return | model = { model | engine = engine, tool = Edit True }, toEngine = v }

                _ ->
                    return

        StopGear ->
            case model.tool of
                Edit _ ->
                    { return | model = { model | engine = Engine.init, tool = Edit False }, toEngine = [ Engine.stop ] }

                _ ->
                    return

        CursorRight ->
            case model.edit of
                [ id ] ->
                    case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                        Content.C col ->
                            { return | model = { model | beadCursor = min (model.beadCursor + 1) <| Collar.length col } }

                        _ ->
                            return

                _ ->
                    return

        CursorLeft ->
            case model.edit of
                [ id ] ->
                    case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                        Content.C _ ->
                            { return | model = { model | beadCursor = max (model.beadCursor - 1) 0 } }

                        _ ->
                            return

                _ ->
                    return

        NewBead c ->
            case addBead model mobile <| Collar.beadFromContent c of
                Just ( newModel, newMobile, id ) ->
                    { return
                        | model = newModel
                        , mobile = newMobile
                        , toUndo = Group
                        , cmd = Random.generate (\color -> WheelMsgs [ ( ( id, [ model.beadCursor ] ), Wheel.ChangeColor color ) ]) colorGen
                    }

                _ ->
                    return

        UnpackBead ( w, l ) new ->
            if new then
                case addBead model mobile { wheel = w, length = l } of
                    Just ( newModel, newMobile, _ ) ->
                        { return
                            | model = newModel
                            , mobile = newMobile
                            , toUndo = Do
                        }

                    _ ->
                        return

            else
                Debug.todo "ChangeContent of bead, has to select bead"

        {- case model.common.edit of
           [ B i ] ->
               update (WheelMsg ( i, Wheel.ChangeContent <| Wheel.getContent { wheel = w } )) ( model, collar )

           _ ->
               return
        -}
        CopyGear harmo id ->
            let
                d =
                    vec2 (Mobile.getLengthId id mobile.gears * 1.1) 0
            in
            { return | mobile = { mobile | gears = Mobile.copy harmo d id mobile.gears }, toUndo = Do }

        CopyContent w ->
            case model.edit of
                [ id ] ->
                    doChangeContent id (Wheel.getWheelContent w) (Just w.color) model mobile

                _ ->
                    return

        NewGear p content ->
            let
                ( id, gears ) =
                    Coll.insertTellId (Mobile.gearFromContent content p) mobile.gears

                svg =
                    PanSvg.centerZoom (Mobile.gearPosSize id gears) model.svg
            in
            { return
                | mobile = { mobile | gears = gears }
                , toUndo = Group
                , model = { model | svg = svg, pack = Pack.update (Pack.PrepareZoom svg) model.pack }
                , cmd = Random.generate (\color -> WheelMsgs [ ( ( id, [] ), Wheel.ChangeColor color ) ]) colorGen
            }

        DeleteWheel ( id, l ) ->
            let
                ( tmp, toUncollar ) =
                    CommonData.deleteWheel ( id, l ) mobile Mobile.rm Collar.rm

                newMob =
                    if toUncollar then
                        Maybe.withDefault tmp <|
                            Maybe.map Tuple.first <|
                                uncollar ( id, List.take (List.length l - 1) l ) tmp

                    else
                        tmp
            in
            { return
                | model =
                    { model
                        | edit =
                            if l == [] && id /= mobile.motor then
                                List.filter ((/=) id) model.edit

                            else
                                model.edit
                        , beadCursor =
                            if model.edit == [ id ] then
                                case l of
                                    [ i ] ->
                                        case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                                            Content.C _ ->
                                                if model.beadCursor > i then
                                                    model.beadCursor - 1

                                                else
                                                    model.beadCursor

                                            _ ->
                                                model.beadCursor

                                    _ ->
                                        model.beadCursor

                            else
                                model.beadCursor
                        , engine = Engine.init
                    }
                , toUndo = Do
                , toEngine = [ Engine.stop ]
                , mobile = newMob
            }

        EnteredFract isNumerator str ->
            Maybe.map2 Tuple.pair model.link (toIntOrEmpty str)
                |> Maybe.map
                    (\( link, iOr ) ->
                        { return
                            | model =
                                { model
                                    | link =
                                        Just
                                            { link
                                                | fractInput =
                                                    case link.fractInput of
                                                        FractionInput fract numB denB ->
                                                            case iOr of
                                                                Empty ->
                                                                    FractionInput fract (not isNumerator) isNumerator

                                                                Int i ->
                                                                    if isNumerator then
                                                                        FractionInput { fract | num = i } True denB

                                                                    else
                                                                        FractionInput { fract | den = i } numB True

                                                        TextInput s ->
                                                            TextInput s
                                            }
                                }
                        }
                    )
                |> Maybe.withDefault return

        AppliedFract l fract ->
            let
                newFract =
                    Fract.multiplication fract <| Harmo.getFract <| Coll.get (Tuple.first l) mobile.gears
            in
            { return
                | mobile = { mobile | gears = Coll.update (Tuple.second l) (Harmo.setFract newFract) mobile.gears }
                , toUndo = Do
            }

        EnteredTextFract str ->
            if String.all (\c -> Char.isDigit c || c == '/') str then
                case model.link of
                    Nothing ->
                        return

                    Just link ->
                        case link.fractInput of
                            FractionInput _ _ _ ->
                                return

                            TextInput _ ->
                                { return | model = { model | link = Just { link | fractInput = TextInput str } } }

            else
                return

        ForcedFract l fract ->
            -- TODO FIXME URGENTLY Abuses Harmo internals, as Gear.copy
            let
                harmoFrom =
                    (Coll.get (Tuple.first l) mobile.gears).harmony

                newBase =
                    Maybe.withDefault (Tuple.first l) <| Harmo.getBaseId harmoFrom

                newOther =
                    Harmo.Other <| Coll.idMap newBase

                newFract =
                    Fract.multiplication harmoFrom.fract fract

                harmoTo =
                    (Coll.get (Tuple.second l) mobile.gears).harmony

                harmonics =
                    case harmoTo.ref of
                        Harmo.Self r ->
                            r.group

                        Harmo.Other _ ->
                            []

                links =
                    case harmoTo.ref of
                        Harmo.Self r ->
                            r.links

                        Harmo.Other _ ->
                            []
            in
            { return
                | mobile =
                    { mobile
                        | gears =
                            List.foldl
                                (\harmonic ->
                                    Coll.update harmonic <|
                                        \g ->
                                            { g
                                                | harmony =
                                                    { ref = newOther
                                                    , fract =
                                                        Fract.division
                                                            (Fract.multiplication g.harmony.fract newFract)
                                                            harmoTo.fract
                                                    }
                                            }
                                )
                                (mobile.gears
                                    |> Coll.update (Tuple.second l)
                                        (\g -> { g | harmony = { ref = newOther, fract = newFract } })
                                    |> Coll.update newBase
                                        (\g ->
                                            let
                                                harmony =
                                                    g.harmony
                                            in
                                            case harmony.ref of
                                                Harmo.Self r ->
                                                    { g
                                                        | harmony =
                                                            { harmony
                                                                | ref =
                                                                    Harmo.Self
                                                                        { r
                                                                            | group =
                                                                                Coll.idMap (Tuple.second l)
                                                                                    :: r.group
                                                                                    ++ harmonics
                                                                            , links =
                                                                                Tuple.mapBoth Coll.idMap Coll.idMap l
                                                                                    :: links
                                                                                    ++ r.links
                                                                        }
                                                            }
                                                    }

                                                Harmo.Other _ ->
                                                    Debug.log "IMPOSSIBLE newBase isn’t Self" g
                                        )
                                )
                            <|
                                List.map Coll.idMap harmonics
                    }
                , toUndo = Do
                , model =
                    case model.link of
                        Just link ->
                            { model | link = Just { link | fractInput = FractionInput fract True True } }

                        Nothing ->
                            model
            }

        SimplifyFractView ->
            model.link
                |> Maybe.map
                    (\link ->
                        case link.fractInput of
                            FractionInput fract numB denB ->
                                { return
                                    | model =
                                        { model
                                            | link =
                                                Just
                                                    { link
                                                        | fractInput = FractionInput (Fract.simplify fract) numB denB
                                                    }
                                        }
                                }

                            TextInput _ ->
                                return
                    )
                |> Maybe.withDefault return

        ResizeToContent id ->
            { return
                | mobile =
                    { mobile
                        | gears =
                            Harmo.toContentLength id mobile.gears
                    }
                , toUndo = Do
            }

        {- TODO ResizeToContent bead, has to select bead}
           ResizeToContent i ->
               { return
                   | collar =
                       Collar.updateBead i
                           (\b -> { b | length = CommonData.getContentLength <| Wheel.getContent <| Collar.get i collar })
                           collar
                   , toUndo = Do
               }
        -}
        Capsuled [] ->
            return

        Capsuled (id :: ids) ->
            let
                m =
                    Coll.get id mobile.gears

                newMotor =
                    { m
                        | motor = Motor.default
                        , harmony =
                            Harmo.newRate (Mobile.getLength m mobile.gears / CommonData.getWheeledContentLength m)
                    }

                subMobile =
                    List.foldl
                        (\i acc ->
                            let
                                g =
                                    Coll.get i mobile.gears

                                newG =
                                    { g
                                        | motor = Motor.default
                                        , harmony =
                                            Harmo.newRate
                                                (Mobile.getLength g mobile.gears / CommonData.getWheeledContentLength g)
                                    }
                            in
                            { acc | gears = Coll.insert newG acc.gears }
                        )
                        (Mobile.fromGear newMotor)
                        ids
            in
            { return
                | mobile =
                    { mobile
                        | gears =
                            Coll.update id
                                (Wheel.setContent <| Content.M <| subMobile)
                                mobile.gears
                    }
                , toUndo = Do
            }

        Collared id collaring ->
            let
                g =
                    Coll.get id mobile.gears

                l =
                    Mobile.getLength g mobile.gears

                collar =
                    case collaring of
                        Simple ->
                            Collar.fromWheel g.wheel l

                        Mult i ->
                            Collar.fromWheelMult g.wheel i l

                        Div s i ->
                            Collar.fromSoundDiv s i l

                tmp =
                    Coll.update id (Wheel.setContent <| Content.C collar) mobile.gears
            in
            { return
                | mobile = { mobile | gears = Harmo.toContentLength id tmp }
                , toUndo = Do
            }

        UnCollar id ->
            let
                mayRes =
                    uncollar ( id, [] ) mobile
            in
            Maybe.withDefault return <|
                Maybe.map
                    (\( m, ( newSel, _ ) ) ->
                        { return
                            | mobile = m
                            , toUndo = Do
                            , model = { model | edit = [ newSel ] }
                        }
                    )
                    mayRes

        EnteredCollarMult str ->
            case toIntOrEmpty str of
                Just (Int i) ->
                    if i >= 2 then
                        { return | model = { model | collarMult = ( i, True ) } }

                    else
                        return

                Just Empty ->
                    { return | model = { model | collarMult = Tuple.mapSecond (always False) model.collarMult } }

                _ ->
                    return

        EnteredCollarDiv str ->
            case toIntOrEmpty str of
                Just (Int i) ->
                    if i >= 2 then
                        { return | model = { model | collarDiv = ( i, True ) } }

                    else
                        return

                Just Empty ->
                    { return | model = { model | collarDiv = Tuple.mapSecond (always False) model.collarDiv } }

                _ ->
                    return

        EnteredNewSampleName str ->
            { return
                | model =
                    { model
                        | newSampleName =
                            if String.all (\c -> Char.isAlphaNum c || List.member c charsInFile) str then
                                str

                            else
                                model.newSampleName
                    }
            }

        CutNewSample ->
            case model.edit of
                [ id ] ->
                    let
                        g =
                            Coll.get id mobile.gears
                    in
                    case Wheel.getContent g of
                        Content.S s ->
                            { return | cmd = requestCutSample { fromSoundPath = Sound.getPath s, newFileName = model.newSampleName, percents = Wheel.getLoopPercents g } }

                        _ ->
                            return

                _ ->
                    return

        Blink ->
            case model.dragging of
                Alterning x y ( b, _ ) ->
                    { return
                        | model =
                            { model
                                | dragging =
                                    Alterning x y <|
                                        ( not b
                                        , if b then
                                            blinkOffTime

                                          else
                                            blinkOnTime
                                        )
                            }
                    }

                _ ->
                    return

        WheelMsgs msgs ->
            { return
                | mobile =
                    List.foldl
                        (\( id, subMsg ) gears -> CommonData.updateWheel id subMsg gears)
                        mobile
                        msgs
                , toUndo = Do
            }

        GearMsg ( id, subMsg ) ->
            { return | mobile = { mobile | gears = Coll.update id (Gear.update subMsg) mobile.gears }, toUndo = Do }

        OutMsg subMsg ->
            { return | outMsg = Just subMsg }

        NoMsg ->
            return

        SvgMsg subMsg ->
            let
                svg =
                    PanSvg.update subMsg model.svg
            in
            { return | model = { model | svg = svg, pack = Pack.update (Pack.PrepareZoom svg) model.pack } }

        SVGSize res ->
            case res of
                Result.Err e ->
                    let
                        _ =
                            Debug.log (D.errorToString e) res
                    in
                    return

                Result.Ok s ->
                    let
                        ( wave, cmd ) =
                            Waveform.update (Waveform.GotSize <| floor s.width) model.wave
                    in
                    { return
                        | model =
                            { model
                                | svg = PanSvg.update (PanSvg.ScaleSize 1 s) model.svg
                                , pack = Pack.update (Pack.SvgMsg <| PanSvg.ScaleSize model.pack.scale s) model.pack
                                , wave = wave
                            }
                        , cmd = Cmd.map WaveMsg cmd
                    }

        PackMsg subMsg ->
            { return | model = { model | pack = Pack.update subMsg model.pack } }

        WaveMsg subMsg ->
            let
                ( wave, cmd ) =
                    Waveform.update subMsg model.wave
            in
            { return | model = { model | wave = wave }, cmd = Cmd.map WaveMsg cmd }

        -- TODO use some pattern like outMessage package? or elm-state? elm-return?
        -- TODO move all that in Editor.Interacting, and manage then
        InteractMsg subMsg ->
            let
                ( interact, event ) =
                    Interact.update subMsg model.interact

                newModel =
                    { model | interact = interact }
            in
            case event of
                Nothing ->
                    { return | model = newModel }

                Just e ->
                    case e.action of
                        Interact.DragEnded False ->
                            { return | model = { newModel | dragging = NoDrag }, toUndo = Cancel }

                        _ ->
                            let
                                svgFromZone z =
                                    case z of
                                        ZSurface ->
                                            newModel.svg

                                        ZPack ->
                                            newModel.pack.svg

                                        _ ->
                                            Debug.todo "No pos map if Zone isn’t SVG"

                                toInPos z p =
                                    PanSvg.mapIn p <| svgFromZone z

                                inEvent =
                                    case e.action of
                                        Interact.Dragged info dragZone k ->
                                            let
                                                startZone =
                                                    Tuple.second info.start
                                            in
                                            if
                                                startZone
                                                    == ZWave
                                                    || dragZone
                                                    == ZWave
                                                    || startZone
                                                    == ZWaveMap
                                                    || dragZone
                                                    == ZWaveMap
                                            then
                                                e

                                            else
                                                { e
                                                    | action =
                                                        Interact.Dragged
                                                            { info
                                                                | start = Tuple.mapFirst (toInPos startZone) info.start
                                                                , oldPos = toInPos dragZone info.oldPos
                                                                , newPos = toInPos dragZone info.newPos
                                                                , startD = Vec.scale (PanSvg.getScale <| svgFromZone startZone) info.startD
                                                            }
                                                            dragZone
                                                            k
                                                }

                                        _ ->
                                            e
                            in
                            manageInteractEvent inEvent newModel mobile


subs : Model -> List (Sub Msg)
subs { interact, dragging } =
    [ PanSvg.newSVGSize (SVGSize << D.decodeValue PanSvg.sizeDecoder)
    , Sub.map WaveMsg Waveform.sub
    , gotRecord <| (GotRecord << D.decodeValue D.string)
    ]
        ++ (List.map (Sub.map InteractMsg) <| Interact.subs interact)
        ++ (case dragging of
                Alterning _ _ ( _, t ) ->
                    [ Time.every t <| always <| Blink ]

                _ ->
                    []
           )


viewTools : Model -> Element Msg
viewTools model =
    Input.radioRow [ spacing 30 ]
        { onChange = ChangedTool
        , options =
            [ Input.option (Play False False) <| text "Jeu (W)"
            , Input.option Harmonize <| text "Harmonie (X)"
            , Input.option (Edit False) <| text "Édition (C)"
            ]
        , selected = Just model.tool
        , label = Input.labelHidden "Outils"
        }


viewExtraTools : Model -> Element Msg
viewExtraTools model =
    row [ width fill, padding 20, spacing 20 ]
        (case model.tool of
            Play on rec ->
                [ Input.button [ centerX ]
                    { label =
                        if on then
                            text "Stop"

                        else
                            text "Jouer"
                    , onPress = Just ToggleEngine
                    }
                , Input.button
                    ([ centerX ]
                        ++ (if rec then
                                [ Bg.color (rgb 1 0 0) ]

                            else
                                []
                           )
                    )
                    { label =
                        if rec then
                            text "Cut"

                        else
                            text "Rec"
                    , onPress = Just <| ToggleRecord <| not rec
                    }
                ]

            Edit play ->
                if not <| List.isEmpty model.edit then
                    [ Input.button [ centerX ]
                        { label =
                            if play then
                                text "Stop"

                            else
                                text "Entendre"
                        , onPress =
                            Just <|
                                if play then
                                    StopGear

                                else
                                    PlayGear
                        }
                    ]

                else
                    []

            _ ->
                []
        )


getWavePoints : Model -> Mobeel -> Maybe Waveform.Cursors
getWavePoints model mobile =
    case ( model.tool, model.edit ) of
        ( Edit _, [ id ] ) ->
            let
                g =
                    Coll.get id mobile.gears

                ( start, end ) =
                    Wheel.getLoopPercents g
            in
            case ( g.wheel.viewContent, Wheel.getContent g ) of
                ( True, Content.S s ) ->
                    if Waveform.isDrawn model.wave <| Sound.getPath s then
                        Just <| Waveform.Sound { offset = g.wheel.startPercent, start = start, end = end }

                    else
                        Nothing

                ( True, Content.C c ) ->
                    case c.oneSound of
                        Just oneSound ->
                            if Waveform.isDrawn model.wave oneSound.path then
                                Just <|
                                    Waveform.CollarDiv
                                        { start = oneSound.start
                                        , end = oneSound.end
                                        , divs = oneSound.divs
                                        }

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing



-- TODO Split between mobile view, motor view, harmony view, and whatever else


viewContent : ( Model, Mobeel ) -> Element Msg
viewContent ( model, mobile ) =
    let
        mayWavePoints =
            getWavePoints model mobile

        getMod : Id Geer -> Wheel.Mod
        getMod id =
            if (model.tool == Edit False || model.tool == Edit True) && List.member id model.edit then
                Wheel.Selected <|
                    (List.length model.edit > 1)
                        && ((List.head <| List.reverse model.edit) == Just id)

            else
                case Interact.getInteract model.interact of
                    Just ( IWheel ( iid, _ ), mode ) ->
                        if iid /= id then
                            Wheel.None

                        else
                            case ( model.tool, mode ) of
                                ( Harmonize, Interact.Hover ) ->
                                    Wheel.Resizing

                                ( Edit _, Interact.Hover ) ->
                                    Wheel.Selectable

                                _ ->
                                    Wheel.None

                    Just ( IResizeHandle iid _, mode ) ->
                        if iid /= id then
                            Wheel.None

                        else
                            Wheel.Resizing

                    _ ->
                        Wheel.None

        isWeaving id =
            case model.dragging of
                WeaveBeads _ ids ->
                    List.member id ids

                _ ->
                    False

        {--TODO bead mod inside collar (Wheel.view, viewContent)
        getMod : Int -> Wheel.Mod
        getMod i =
            if model.tool == Edit && model.common.edit == [ B i ] then
                Wheel.Selected False

            else
                case Interact.getInteract model.interact of
                    Just ( IWheel j, Interact.Hover ) ->
                        if B i == j then
                            Wheel.Selectable

                        else
                            Wheel.None

                    _ ->
                        Wheel.None
-}
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.inFront <|
            Pack.view model.pack
                (List.map (Html.Attributes.map InteractMsg) <|
                    Interact.dragSpaceEvents model.interact ZPack
                )
                PackMsg
                IPacked
                IPack
                InteractMsg
        , Element.inFront <|
            Waveform.view
                model.wave
                mayWavePoints
                model.interact
                InteractMsg
                WaveMsg
        ]
    <|
        Element.html <|
            S.svg
                (List.map (Html.Attributes.map SvgMsg) (PanSvg.svgAttributes model.svg)
                    ++ (List.map (Html.Attributes.map InteractMsg) <|
                            Interact.dragSpaceEvents model.interact ZSurface
                                ++ Interact.draggableEvents ISurface
                       )
                )
            <|
                List.map (Svg.map InteractMsg) <|
                    (List.map
                        (\( id, g ) ->
                            let
                                -- TODO should blink also if bead
                                wheel =
                                    g.wheel

                                w =
                                    case model.dragging of
                                        Alterning ( idd, [] ) mayId ( b, _ ) ->
                                            if not b && id == idd then
                                                { wheel | mute = not wheel.mute }

                                            else
                                                case mayId of
                                                    Just ( iid, [] ) ->
                                                        if not b && id == iid then
                                                            { wheel | mute = not wheel.mute }

                                                        else
                                                            wheel

                                                    _ ->
                                                        wheel

                                        _ ->
                                            wheel
                            in
                            Wheel.view w
                                g.pos
                                (Mobile.getLength g mobile.gears)
                                { mod = getMod id
                                , motor = id == mobile.motor
                                , dashed = Harmo.hasHarmonics g.harmony
                                , weaving = isWeaving id
                                , baseColor =
                                    Maybe.map (\bId -> (Coll.get bId mobile.gears).wheel.color) <|
                                        Harmo.getBaseId g.harmony
                                , named =
                                    case Interact.getInteract model.interact of
                                        Just ( IWheel idd, _ ) ->
                                            if id == Tuple.first idd then
                                                Just <| CommonData.getName ( id, [] ) mobile

                                            else
                                                Nothing

                                        _ ->
                                            Nothing
                                }
                                (Just ( IWheel << Tuple.pair id, [] ))
                                (Just <| IResizeHandle id)
                                (model.parentUid ++ Gear.toUID id)
                        )
                     <|
                        Coll.toList mobile.gears
                    )
                        ++ (case model.dragging of
                                HalfLink ( id, pos ) ->
                                    case model.tool of
                                        Play _ _ ->
                                            let
                                                circle =
                                                    Mobile.toCircle mobile.gears id
                                            in
                                            [ Link.drawMotorLink ( circle, { circle | c = pos } ) ]

                                        Harmonize ->
                                            let
                                                g =
                                                    Coll.get id mobile.gears
                                            in
                                            [ Link.drawRawLink
                                                ( g.pos, pos )
                                                (Mobile.getLength g mobile.gears)
                                                Link.baseColor
                                            ]
                                                ++ [ S.g [ SA.opacity <| Opacity 0 ] <|
                                                        List.map
                                                            (\( idd, gg ) ->
                                                                Wheel.view gg.wheel
                                                                    gg.pos
                                                                    (Mobile.getLength gg mobile.gears)
                                                                    Wheel.defaultStyle
                                                                    (Just ( IWheel << Tuple.pair idd, [] ))
                                                                    Nothing
                                                                    ("hoverArtefact-" ++ Gear.toUID idd)
                                                            )
                                                        <|
                                                            Coll.toList mobile.gears
                                                   ]

                                        _ ->
                                            []

                                CompleteLink l ->
                                    case model.tool of
                                        Play _ _ ->
                                            Link.viewMotorLink False <| toDrawLink mobile.gears l

                                        Harmonize ->
                                            Link.viewFractLink (toDrawLink mobile.gears l) <| ILink l

                                        _ ->
                                            []

                                Cut seg _ ->
                                    [ Link.drawCut seg <| PanSvg.getScale model.svg ]

                                WeaveBeads seg _ ->
                                    [ Link.drawCut seg <| PanSvg.getScale model.svg ]

                                Content ( p, l ) ->
                                    [ S.circle
                                        [ SA.cx <| Num <| Vec.getX p
                                        , SA.cy <| Num <| Vec.getY p
                                        , SA.r <| Num (l / 2)
                                        , SA.strokeWidth <| Num <| l / 30
                                        , SA.stroke Color.black
                                        , SA.strokeOpacity <| Opacity 0.5
                                        , SA.fillOpacity <| Opacity 0
                                        ]
                                        []
                                    ]

                                Packed pos id ->
                                    let
                                        p =
                                            Coll.get id model.pack.wheels
                                    in
                                    [ Wheel.view p.wheel pos p.length Wheel.defaultStyle Nothing Nothing "" ]

                                _ ->
                                    []
                           )
                        ++ (case model.tool of
                                Play _ _ ->
                                    let
                                        cuts =
                                            case model.dragging of
                                                Cut _ c ->
                                                    c

                                                _ ->
                                                    []
                                    in
                                    List.concatMap
                                        (\l ->
                                            Link.viewMotorLink (List.any (Link.equal l) cuts) <|
                                                toDrawLink mobile.gears l
                                        )
                                    <|
                                        Motor.getAllLinks mobile.gears

                                Harmonize ->
                                    (case Interact.getInteract model.interact of
                                        Just ( ILink l, _ ) ->
                                            Link.viewFractOnLink (toDrawLink mobile.gears l) <|
                                                Fract.simplify <|
                                                    Fract.division
                                                        (Coll.get (Tuple.second l) mobile.gears).harmony.fract
                                                        (Coll.get (Tuple.first l) mobile.gears).harmony.fract

                                        _ ->
                                            []
                                    )
                                        ++ (List.concatMap (\l -> Link.viewFractLink (toDrawLink mobile.gears l) (ILink l)) <|
                                                List.concatMap (.harmony >> Harmo.getLinks) <|
                                                    Coll.values mobile.gears
                                           )
                                        ++ (case model.link of
                                                Just { link, fractInput } ->
                                                    Link.viewSelectedLink (toDrawLink mobile.gears link) <|
                                                        case fractInput of
                                                            FractionInput _ _ _ ->
                                                                Just <|
                                                                    Fract.simplify <|
                                                                        Fract.division
                                                                            (Coll.get (Tuple.second link) mobile.gears).harmony.fract
                                                                            (Coll.get (Tuple.first link) mobile.gears).harmony.fract

                                                            TextInput _ ->
                                                                Nothing

                                                _ ->
                                                    []
                                           )

                                Edit _ ->
                                    case model.edit of
                                        [ id ] ->
                                            let
                                                g =
                                                    Coll.get id mobile.gears

                                                length =
                                                    Mobile.getLength g mobile.gears

                                                pos =
                                                    g.pos

                                                w =
                                                    g.wheel
                                            in
                                            case Wheel.getWheelContent w of
                                                Content.C col ->
                                                    let
                                                        medLength =
                                                            Collar.getMinLength col + Collar.getMaxLength col / 2

                                                        cursorW =
                                                            medLength / 15

                                                        cursorH =
                                                            medLength * 2

                                                        scale =
                                                            length / Content.getMatriceLength col
                                                    in
                                                    [ S.rect
                                                        [ SA.transform [ Translate (getX pos) (getY pos), Translate (-length / 2) 0, Scale scale scale ]
                                                        , SA.x <| Num <| Collar.getCumulLengthAt model.beadCursor col - cursorW / 2
                                                        , SA.y <| Num <| -cursorH / 2
                                                        , SA.width <| Num cursorW
                                                        , SA.height <| Num cursorH
                                                        , SA.fill <| Fill Color.lightBlue
                                                        ]
                                                        []
                                                    ]

                                                _ ->
                                                    []

                                        _ ->
                                            []
                           )



-- TODO split in functions for each component, and maybe move to another file, like Interacting, or good old common


viewDetailsColumn : Color -> List (Element msg) -> Element msg
viewDetailsColumn bg =
    column
        [ height fill
        , Bg.color bg
        , Font.color (rgb 1 1 1)
        , Font.size 16
        , spacing 20
        , padding 10
        ]


viewDetails : Model -> Mobeel -> List (Element Msg)
viewDetails model mobile =
    case model.mode of
        ChangeSound id ->
            [ viewDetailsColumn (rgb 0.5 0.2 0) <|
                [ text <| CommonData.getName ( id, [] ) mobile
                , text "Choisir un son chargé"
                , Input.button []
                    { label = text "Annuler"
                    , onPress = Just <| ChangedMode Normal
                    }
                ]
            ]

        SelectMotor ->
            [ viewDetailsColumn (rgb 0.5 0.2 0) <|
                [ text "Choisir nouvelle Motrice"
                , Input.button []
                    { label = text "Annuler"
                    , onPress = Just <| ChangedMode Normal
                    }
                ]
            ]

        _ ->
            case model.tool of
                Edit _ ->
                    viewEditDetails model mobile

                Harmonize ->
                    viewHarmonizeDetails model mobile

                _ ->
                    []


viewEditDetails : Model -> Mobeel -> List (Element Msg)
viewEditDetails model mobile =
    case model.edit of
        [ id ] ->
            let
                g =
                    Coll.get id mobile.gears

                wId =
                    ( id, [] )
            in
            [ viewDetailsColumn (rgb 0.5 0.5 0.5) <|
                [ Input.text [ Font.color (rgb 0 0 0) ]
                    { label = Input.labelAbove [] <| text "Roue :"
                    , text = g.wheel.name
                    , placeholder = Just <| Input.placeholder [] <| text <| CommonData.getName ( id, [] ) mobile
                    , onChange = \str -> WheelMsgs [ ( wId, Wheel.Named str ) ]
                    }
                , case Wheel.getContent g of
                    Content.S s ->
                        Input.button []
                            { label =
                                text <|
                                    if g.wheel.viewContent then
                                        "Ranger " ++ Sound.getPath s

                                    else
                                        "Voir " ++ Sound.getPath s
                            , onPress = Just <| WheelMsgs [ ( wId, Wheel.ToggleContentView ) ]
                            }

                    Content.M _ ->
                        Input.button []
                            { label = text "Voir Mobile"
                            , onPress = Just <| OutMsg <| Inside wId
                            }

                    Content.C _ ->
                        Input.button []
                            { label =
                                text <|
                                    if g.wheel.viewContent then
                                        "Replier Collier"

                                    else
                                        "Déplier Collier"
                            , onPress = Just <| WheelMsgs [ ( wId, Wheel.ToggleContentView ) ]
                            }
                , column [ width fill, scrollbarY, spacing 20, padding 10 ] <|
                    [ Input.slider
                        [ behindContent <|
                            el
                                [ width fill
                                , height <| px 2
                                , centerY
                                , Bg.color <| rgb 0 0 0
                                , Border.rounded 2
                                ]
                                Element.none
                        ]
                        { label = Input.labelAbove [] <| text "Volume"
                        , onChange = \f -> WheelMsgs [ ( wId, Wheel.ChangeVolume f ) ]
                        , value = g.wheel.volume
                        , min = 0
                        , max = 1
                        , step = Just 0.01
                        , thumb = Input.defaultThumb
                        }
                    , row [ spacing 16 ] <|
                        text "x"
                            :: List.map
                                (\i ->
                                    Input.button []
                                        { label = text <| String.fromInt i
                                        , onPress = Just <| GearMsg ( id, Gear.ResizeFract <| Fract.integer i )
                                        }
                                )
                                [ 2, 3, 5, 7 ]
                    , row [ spacing 16 ] <|
                        text "/"
                            :: List.map
                                (\i ->
                                    Input.button []
                                        { label = text <| String.fromInt i
                                        , onPress = Just <| GearMsg ( id, Gear.ResizeFract <| Fract.unit i )
                                        }
                                )
                                [ 2, 3, 5, 7 ]
                    , Input.button []
                        { label = text "Taille Originale"
                        , onPress = Just <| ResizeToContent id
                        }
                    , Input.button []
                        { label = text "Changer son"
                        , onPress = Just <| ChangedMode <| ChangeSound id
                        }
                    , Input.button []
                        { label = text "Encapsuler"
                        , onPress = Just <| Capsuled [ id ]
                        }
                    , let
                        btns label pressMsg changeMsg tuple =
                            Input.button []
                                { label = text label
                                , onPress = Just <| Collared id <| pressMsg <| Tuple.first tuple
                                }
                                :: [ Input.text
                                        [ paddingXY 0 0
                                        , width <| minimum 40 <| fill
                                        , Font.color <| rgb 0 0 0
                                        , htmlAttribute <| Html.Attributes.type_ "number"
                                        , htmlAttribute <| Html.Attributes.min "2"
                                        ]
                                        { onChange = changeMsg
                                        , text =
                                            if Tuple.second tuple then
                                                String.fromInt <| Tuple.first tuple

                                            else
                                                ""
                                        , placeholder =
                                            Just <|
                                                Input.placeholder [] <|
                                                    text <|
                                                        String.fromInt <|
                                                            Tuple.first tuple
                                        , label = Input.labelHidden <| "Collar " ++ label
                                        }
                                   ]

                        multBtns =
                            btns "x" Mult EnteredCollarMult model.collarMult

                        divBtns s =
                            btns "/" (Div s) EnteredCollarDiv model.collarDiv

                        simpleBtn =
                            Input.button []
                                { label = text "Collier"
                                , onPress = Just <| Collared id Simple
                                }
                      in
                      case Wheel.getContent g of
                        Content.S s ->
                            row [ spacing 16 ] <|
                                simpleBtn
                                    :: multBtns
                                    ++ divBtns s

                        _ ->
                            row [ spacing 16 ] <|
                                simpleBtn
                                    :: multBtns
                    , Input.button []
                        { label = text "Décollier"
                        , onPress = Just <| UnCollar id
                        }
                    , if id == mobile.motor then
                        Input.button []
                            { onPress = Just <| ChangedMode SelectMotor
                            , label = text "Changer Motrice"
                            }

                      else
                        Input.button []
                            { onPress = Just <| DeleteWheel ( id, [] )
                            , label = text "Supprimer"
                            }
                    ]
                        ++ (List.map (Element.map PackMsg) <| Pack.viewPackButtons model.pack)
                        ++ [ Input.button []
                                { label = text "Copier Contenu"
                                , onPress =
                                    Maybe.map (\i -> PackMsg <| Pack.Packontent <| (Coll.get i mobile.gears).wheel) <|
                                        case model.edit of
                                            [ one ] ->
                                                Just one

                                            _ ->
                                                Nothing
                                }
                           ]
                        ++ (case model.pack.content of
                                Just w ->
                                    [ Input.button []
                                        { label = text <| "Coller Contenu"
                                        , onPress = Just <| CopyContent w
                                        }
                                    ]

                                Nothing ->
                                    []
                           )
                        ++ [ row []
                                [ text "Couleur : "
                                , html <|
                                    Html.input
                                        [ Html.Attributes.type_ "color"
                                        , Html.Attributes.value <| colorToString <| Color.hsl g.wheel.color 1 0.5
                                        , Html.Events.onInput
                                            (\str ->
                                                WheelMsgs
                                                    [ ( wId
                                                      , Wheel.ChangeColor <|
                                                            (Color.toHsla <|
                                                                Color.fromRgba
                                                                    { red = hexToFloat <| String.slice 1 3 str
                                                                    , green = hexToFloat <| String.slice 3 5 str
                                                                    , blue = hexToFloat <| String.slice 5 7 str
                                                                    , alpha = 1
                                                                    }
                                                            ).hue
                                                      )
                                                    ]
                                            )

                                        -- TODO should Group undo with onChange event as final Do
                                        ]
                                        []
                                ]
                           ]
                , text <|
                    "Durée : "
                        ++ Harmo.view id
                            mobile.gears
                            (\rId ->
                                CommonData.getName ( rId, [] ) mobile
                            )
                , text <| "( " ++ (Round.round 2 <| Mobile.getLengthId id mobile.gears) ++ " )"
                , text <|
                    "Contenu : "
                        ++ (Round.round 2 <| CommonData.getWheeledContentLength g)
                ]
                    ++ (case Wheel.getContent g of
                            Content.S s ->
                                let
                                    ok =
                                        model.newSampleName
                                            /= Sound.getPath s
                                            && (not <| String.isEmpty model.newSampleName)
                                in
                                [ Input.button
                                    [ Font.color <|
                                        if ok then
                                            rgb 1 1 1

                                        else
                                            rgb 0 0 0
                                    ]
                                    { label = text "Couper en tant que"
                                    , onPress =
                                        if ok then
                                            Just <| CutNewSample

                                        else
                                            Nothing
                                    }
                                , Input.text [ Font.color <| rgb 0 0 0 ]
                                    { label = Input.labelHidden "Nouveau nom"
                                    , text = model.newSampleName
                                    , placeholder = Just <| Input.placeholder [] <| text "Nouveau nom"
                                    , onChange = EnteredNewSampleName
                                    }
                                ]

                            _ ->
                                []
                       )
            ]

        _ :: _ ->
            [ viewDetailsColumn (rgb 0.5 0.5 0.5) <|
                (List.map (\id -> text <| CommonData.getName ( id, [] ) mobile) <| List.reverse model.edit)
                    ++ [ Input.button []
                            { label = text "Encapsuler"
                            , onPress = Just <| Capsuled <| List.reverse model.edit
                            }
                       ]
            ]

        _ ->
            []


viewHarmonizeDetails : Model -> Mobeel -> List (Element Msg)
viewHarmonizeDetails model mobile =
    case model.link of
        Just { link, fractInput } ->
            [ viewDetailsColumn (rgb 0.5 0.5 0.5)
                ([ text <| (Gear.toUID <| Tuple.first link) ++ (Gear.toUID <| Tuple.second link) ]
                    ++ (case fractInput of
                            FractionInput fract numB denB ->
                                [ Input.text [ Font.color (rgb 0 0 0) ]
                                    { text =
                                        if numB then
                                            String.fromInt fract.num

                                        else
                                            ""
                                    , onChange = EnteredFract True
                                    , label = Input.labelHidden "Numerator"
                                    , placeholder = Just <| Input.placeholder [] <| text <| String.fromInt fract.num
                                    }
                                , text "/"
                                , Input.text [ Font.color (rgb 0 0 0) ]
                                    { text =
                                        if denB then
                                            String.fromInt fract.den

                                        else
                                            ""
                                    , onChange = EnteredFract False
                                    , label = Input.labelHidden "Denominator"
                                    , placeholder = Just <| Input.placeholder [] <| text <| String.fromInt fract.den
                                    }
                                , Input.button []
                                    { label = text "Change"
                                    , onPress = Just <| AppliedFract link fract
                                    }
                                , Input.button []
                                    { label = text "Simplifier"
                                    , onPress = Just SimplifyFractView
                                    }
                                ]

                            TextInput str ->
                                let
                                    mayFract =
                                        case String.toInt <| String.trim str of
                                            Just num ->
                                                Just <| Fract.integer num

                                            Nothing ->
                                                case List.map (String.toInt << String.trim) <| String.split "/" str of
                                                    [ Just num, Just den ] ->
                                                        Just { num = num, den = den }

                                                    _ ->
                                                        Nothing
                                in
                                [ Input.text [ Font.color (rgb 0 0 0) ]
                                    { placeholder =
                                        Just <|
                                            Input.placeholder [] <|
                                                text <|
                                                    Round.round
                                                        5
                                                    <|
                                                        Mobile.getLengthId (Tuple.second link) mobile.gears
                                                            / Mobile.getLengthId (Tuple.first link) mobile.gears
                                    , text = str
                                    , label = Input.labelHidden "Fraction"
                                    , onChange = EnteredTextFract
                                    }
                                , Input.button
                                    (case mayFract of
                                        Just _ ->
                                            []

                                        Nothing ->
                                            [ Font.color <| rgb 1 0 0 ]
                                    )
                                    { label = text "Forcer"
                                    , onPress = Maybe.map (ForcedFract link) mayFract
                                    }
                                ]
                       )
                    ++ [ row [] [] ]
                )
            ]

        Nothing ->
            []


doLinked : Link Geer -> Coll Geer -> ( Coll Geer, Maybe Fraction )
doLinked l gears =
    let
        base id =
            Maybe.withDefault id <| Harmo.getBaseId (Coll.get id gears).harmony

        bases =
            Tuple.mapBoth base base l
    in
    ( if
        (Tuple.first bases == Tuple.second bases)
            && (not <| Harmo.isActiveLink l (Coll.get (Tuple.first bases) gears).harmony)
      then
        Coll.update (Tuple.first bases) (Harmo.addLink l) gears

      else
        gears
    , if Tuple.first bases == Tuple.second bases then
        Just <|
            Fract.division
                (Coll.get (Tuple.second l) gears).harmony.fract
                (Coll.get (Tuple.first l) gears).harmony.fract

      else
        Nothing
    )


doVolumeChange :
    Identifier
    -> Vec2
    -> Mobeel
    -> Engine
    -> { mobile : Mobeel, toUndo : ToUndo, toEngine : List E.Value }
doVolumeChange id absD mobile engine =
    let
        volume =
            (CommonData.getWheel id mobile).volume - Vec.getY absD / 100
    in
    { mobile = CommonData.updateWheel id (Wheel.ChangeVolume volume) mobile
    , toUndo = Group
    , toEngine = Engine.volumeChanged id volume engine
    }


doResize : Id Geer -> Vec2 -> Bool -> Mobeel -> Mobeel
doResize id d add mobile =
    let
        gears =
            mobile.gears

        length =
            Mobile.getLengthId id gears

        dd =
            if add then
                Vec.getX d

            else
                -1 * Vec.getX d

        newSize =
            abs <| dd * 2 + length
    in
    { mobile | gears = Harmo.resizeFree id newSize (CommonData.getWheeledContentLength <| Coll.get id gears) gears }


doChangeContent : Id Geer -> Conteet -> Maybe Float -> Model -> Mobeel -> Return
doChangeContent id c mayColor model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = []
            , outMsg = Nothing
            , cmd = Cmd.none
            }

        group =
            Harmo.getHarmonicGroup (Coll.idMap id) mobile.gears

        chSound =
            Wheel.update <| Wheel.ChangeContent c

        tmpGears =
            Harmo.changeContentKeepLength id
                (CommonData.getContentLength c)
                (CommonData.getWheeledContentLength <| Coll.get id mobile.gears)
                mobile.gears

        gears =
            List.foldl (\el -> Coll.update el chSound) tmpGears group
    in
    case mayColor of
        Just color ->
            let
                chColor =
                    Wheel.update <| Wheel.ChangeColor color
            in
            { return
                | mobile = { mobile | gears = List.foldl (\el -> Coll.update el chColor) gears group }
                , toUndo = Do
            }

        Nothing ->
            let
                colorToMsgs color =
                    List.map (\el -> ( ( el, [] ), Wheel.ChangeColor color )) group
            in
            { return
                | mobile = { mobile | gears = gears }
                , toUndo = Group
                , cmd = Random.generate (WheelMsgs << colorToMsgs) colorGen
            }


addBead : Model -> Mobeel -> Beed -> Maybe ( Model, Mobeel, Id Geer )
addBead model mobile bead =
    case model.edit of
        [ id ] ->
            case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                Content.C col ->
                    let
                        newCol =
                            Collar.add model.beadCursor bead col

                        newMob =
                            CommonData.updateWheel ( id, [] )
                                (Wheel.ChangeContent <| Content.C newCol)
                                mobile
                    in
                    Just
                        ( { model | beadCursor = model.beadCursor + 1 }
                        , newMob
                        , id
                        )

                _ ->
                    Nothing

        _ ->
            Nothing


uncollar : Identifier -> Mobeel -> Maybe ( Mobeel, Identifier )
uncollar id m =
    let
        w =
            CommonData.getWheel id m
    in
    case Wheel.getWheelContent w of
        Content.C col ->
            let
                beads =
                    Collar.getBeads col

                contentLength =
                    Collar.getTotalLength col

                ( gId, listPos ) =
                    id
            in
            case List.reverse listPos of
                [] ->
                    let
                        nextId =
                            Tuple.first <| Coll.insertTellId (Gear.default Wheel.default) m.gears

                        newMotor =
                            if m.motor == gId then
                                nextId

                            else
                                m.motor

                        newMob =
                            Tuple.first <| CommonData.deleteWheel id { m | motor = newMotor } Mobile.rm Collar.rm

                        ( gearPos, gearLength ) =
                            Mobile.gearPosSize gId m.gears

                        xMin =
                            Vec.getX gearPos - gearLength / 2

                        ratio =
                            gearLength / contentLength

                        newGears =
                            Tuple.first <|
                                List.foldl
                                    (\{ length, wheel } ( gears, x ) ->
                                        let
                                            realLength =
                                                length * ratio
                                        in
                                        ( gears
                                            |> Coll.insert
                                                { motor = Motor.default
                                                , wheel = wheel
                                                , harmony = Harmo.newRate <| realLength / CommonData.getWheeledContentLength { wheel = wheel }
                                                , pos = Vec.setX (x + realLength / 2) gearPos
                                                }
                                        , x + realLength
                                        )
                                    )
                                    ( newMob.gears, xMin )
                                    beads
                    in
                    Just ( { newMob | gears = newGears }, ( nextId, [] ) )

                lastIndex :: revRest ->
                    let
                        upColId =
                            ( gId, List.reverse revRest )
                    in
                    case Wheel.getWheelContent <| CommonData.getWheel upColId m of
                        Content.C upCol ->
                            Just
                                ( CommonData.updateWheel upColId
                                    (Wheel.ChangeContent <|
                                        Content.C <|
                                            Collar.addBeads lastIndex beads upCol
                                    )
                                    m
                                , id
                                )

                        _ ->
                            Nothing

        _ ->
            Nothing


toDrawLink : Coll Geer -> Link Geer -> DrawLink
toDrawLink gears =
    let
        toC =
            Mobile.toCircle gears
    in
    Tuple.mapBoth toC toC


computeCuts : Segment -> Coll Geer -> List (Link Geer) -> List (Link Geer)
computeCuts cut gears =
    List.filter <| Link.cuts cut << Link.toSegment << toDrawLink gears


computeTouch : Segment -> Coll Geer -> List (Id Geer)
computeTouch weave gears =
    List.filter (Link.touchCircle weave << Mobile.toCircle gears) <| Coll.ids gears



-- TODO Maybe forward straight away to Pack if event.item is IPack ?


manageInteractEvent : Interact.Event Interactable Zone -> Model -> Mobeel -> Return
manageInteractEvent event model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = []
            , outMsg = Nothing
            , cmd = Cmd.none
            }
    in
    -- TODO Find good pattern for big mess there
    case model.mode of
        Nav ->
            case ( event.item, event.action ) of
                ( IWheel id, Interact.Clicked _ ) ->
                    case Wheel.getWheelContent <| CommonData.getWheel id mobile of
                        Content.M _ ->
                            { return | outMsg = Just <| Inside id }

                        _ ->
                            update (WheelMsgs [ ( id, Wheel.ToggleContentView ) ]) ( model, mobile )

                _ ->
                    return

        Move ->
            case interactMove event model mobile of
                Just ret ->
                    ret

                _ ->
                    case ( event.item, event.action ) of
                        ( ISurface, Interact.Dragged { startD } _ _ ) ->
                            { return | model = { model | svg = PanSvg.update (PanSvg.Move startD) model.svg } }

                        _ ->
                            return

        SelectMotor ->
            case ( event.item, event.action ) of
                ( IWheel ( id, [] ), Interact.Clicked _ ) ->
                    { return
                        | model = { model | mode = Normal }
                        , mobile = { mobile | motor = id }
                        , toUndo = Do
                    }

                _ ->
                    return

        ChangeSound id ->
            case ( event.item, event.action ) of
                ( ISound s, Interact.Clicked _ ) ->
                    doChangeContent id (Content.S s) Nothing { model | mode = Normal } mobile

                _ ->
                    return

        SupprMode ->
            case ( event.item, event.action ) of
                ( IWheel id, Interact.Clicked _ ) ->
                    update (DeleteWheel id) ( model, mobile )

                ( IPacked id, Interact.Clicked _ ) ->
                    update (PackMsg <| Pack.Unpack id) ( model, mobile )

                _ ->
                    return

        Solo ->
            case model.tool of
                Play _ _ ->
                    case ( event.item, event.action ) of
                        ( IWheel ( id, [] ), Interact.Holded ) ->
                            -- TODO should work also for beads (not []), Mobile.mapWheels ?
                            let
                                newMobile =
                                    List.foldl
                                        (\( idd, g ) -> CommonData.updateWheel ( idd, [] ) <| (Wheel.Mute <| idd /= id))
                                        mobile
                                    <|
                                        Coll.toList mobile.gears
                            in
                            { return
                                | mobile = newMobile
                                , toUndo = Group
                                , toEngine = updateAllMuteToEngine model newMobile
                            }

                        ( _, Interact.HoldEnded ) ->
                            { return | toUndo = Cancel, outMsg = Just UnSolo }

                        ( _, Interact.DragEnded _ ) ->
                            { return | toUndo = Cancel, outMsg = Just UnSolo }

                        _ ->
                            return

                _ ->
                    return

        Clone ->
            -- TODO hover show pos of new wheel, which is linked to mouse pos to center of wheel
            case ( event.item, event.action ) of
                ( IWheel ( id, [] ), Interact.Clicked _ ) ->
                    update (CopyGear False id) ( model, mobile )

                _ ->
                    return

        Alternate ->
            case model.tool of
                Play _ _ ->
                    case ( event.item, event.action, model.dragging ) of
                        ( IWheel id, Interact.Dragged _ _ _, NoDrag ) ->
                            { return | model = { model | dragging = Alterning id Nothing ( False, blinkOffTime ) } }

                        ( IWheel id, Interact.DragIn, Alterning other _ b ) ->
                            { return | model = { model | dragging = Alterning other (Just id) b } }

                        ( IWheel _, Interact.DragOut, Alterning other _ b ) ->
                            { return
                                | model =
                                    { model
                                        | dragging = Alterning other Nothing b
                                    }
                            }

                        ( _, Interact.DragEnded True, Alterning id1 (Just id2) _ ) ->
                            let
                                newMobile =
                                    List.foldl
                                        (\id mob -> CommonData.updateWheel id Wheel.ToggleMute mob)
                                        mobile
                                        [ id1, id2 ]
                            in
                            { return
                                | model = { model | dragging = NoDrag }
                                , mobile = newMobile
                                , toUndo = Do
                                , toEngine =
                                    List.concatMap
                                        (\id ->
                                            Engine.muted id (CommonData.getWheel id newMobile).mute model.engine
                                        )
                                        [ id1, id2 ]
                            }

                        ( _, Interact.DragEnded _, _ ) ->
                            { return | model = { model | dragging = NoDrag } }

                        _ ->
                            return

                _ ->
                    return

        Normal ->
            case ( event.item, event.action, model.dragging ) of
                -- FROM SOUNDLIST
                ( ISound s, Interact.Clicked _, _ ) ->
                    case model.edit of
                        [ id ] ->
                            case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                                Content.C _ ->
                                    case model.tool of
                                        Edit _ ->
                                            update (NewBead <| Content.S s) ( model, mobile )

                                        _ ->
                                            update (NewGear defaultAddPos <| Content.S s) ( model, mobile )

                                _ ->
                                    update (NewGear defaultAddPos <| Content.S s) ( model, mobile )

                        _ ->
                            update (NewGear defaultAddPos <| Content.S s) ( model, mobile )

                ( ISound s, Interact.Dragged { newPos } ZSurface _, _ ) ->
                    { return
                        | model = { model | dragging = Content ( newPos, Sound.length s ) }
                    }

                ( ISound s, Interact.DragEnded True, Content ( p, _ ) ) ->
                    update (NewGear p <| Content.S s) ( { model | dragging = NoDrag }, mobile )

                -- FROM PACK
                ( IPacked pId, Interact.Clicked _, _ ) ->
                    case model.edit of
                        [ id ] ->
                            case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                                Content.C _ ->
                                    case model.tool of
                                        Edit _ ->
                                            let
                                                p =
                                                    Coll.get pId model.pack.wheels
                                            in
                                            update (UnpackBead ( p.wheel, p.length ) True) ( model, mobile )

                                        _ ->
                                            return

                                _ ->
                                    return

                        _ ->
                            return

                ( IPacked id, Interact.Dragged { newPos } ZPack _, _ ) ->
                    { return
                        | model =
                            { model | dragging = NoDrag, pack = Pack.update (Pack.DragFrom id newPos) model.pack }
                    }

                ( IPacked id, Interact.Dragged { newPos } ZSurface _, _ ) ->
                    { return
                        | model =
                            { model | dragging = Packed newPos id, pack = Pack.update (Pack.InitDrag id) model.pack }
                    }

                ( IPacked id, Interact.DragEnded True, Packed pos _ ) ->
                    let
                        p =
                            Coll.get id model.pack.wheels
                    in
                    { return
                        | model = { model | dragging = NoDrag }
                        , mobile =
                            { mobile | gears = Coll.insert (Mobile.newSizedGear pos p.length p.wheel) mobile.gears }
                        , toUndo = Do
                    }

                -- Pack Surface
                ( IPack, Interact.Dragged { startD } _ _, _ ) ->
                    { return
                        | model =
                            { model
                                | pack = Pack.update (Pack.SvgMsg <| PanSvg.Move startD) model.pack
                            }
                    }

                _ ->
                    case model.tool of
                        -- PLAY --------
                        Play on _ ->
                            interactPlay on event model mobile

                        -- LINK --------
                        Harmonize ->
                            interactHarmonize event model mobile

                        -- EDIT --------
                        Edit _ ->
                            case interactMove event model mobile of
                                Just ret ->
                                    ret

                                Nothing ->
                                    case interactEdit event model mobile of
                                        Just { newModel, newMobile, toUndo, cmd } ->
                                            let
                                                ret =
                                                    update StopGear ( newModel, newMobile )
                                            in
                                            { ret | toUndo = toUndo, cmd = Cmd.batch [ cmd, ret.cmd ] }

                                        Nothing ->
                                            case model.edit of
                                                [ id ] ->
                                                    let
                                                        g =
                                                            Coll.get id mobile.gears
                                                    in
                                                    case interactWave g event model mobile of
                                                        Just (ReturnWheel subMsg) ->
                                                            update (WheelMsgs [ ( ( id, [] ), subMsg ) ]) ( model, mobile )

                                                        Just (ReturnWave subMsg) ->
                                                            update (WaveMsg subMsg) ( model, mobile )

                                                        Nothing ->
                                                            return

                                                _ ->
                                                    return


interactPlay : Bool -> Interact.Event Interactable Zone -> Model -> Mobeel -> Return
interactPlay on event model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = []
            , outMsg = Nothing
            , cmd = Cmd.none
            }
    in
    case ( event.item, event.action, model.dragging ) of
        -- MUTE
        ( IWheel id, Interact.Clicked _, _ ) ->
            let
                newMobile =
                    CommonData.updateWheel id Wheel.ToggleMute mobile
            in
            { return
                | mobile = newMobile
                , toUndo = Do
                , toEngine = Engine.muted id (CommonData.getWheel id newMobile).mute model.engine
            }

        -- CUT
        ( ISurface, Interact.Dragged { oldPos, newPos } ZSurface _, NoDrag ) ->
            { return
                | model =
                    { model
                        | dragging =
                            Cut ( oldPos, newPos ) <|
                                computeCuts ( oldPos, newPos ) mobile.gears <|
                                    Motor.getAllLinks mobile.gears
                    }
            }

        ( ISurface, Interact.Dragged { newPos } ZSurface _, Cut ( p1, _ ) _ ) ->
            { return
                | model =
                    { model
                        | dragging =
                            Cut ( p1, newPos ) <|
                                computeCuts ( p1, newPos ) mobile.gears <|
                                    Motor.getAllLinks mobile.gears
                    }
            }

        ( ISurface, Interact.DragEnded True, Cut _ cuts ) ->
            let
                ( gears, motored ) =
                    Motor.remove cuts mobile.motor mobile.gears on

                ( engine, v ) =
                    Engine.setPlaying motored mobile.gears model.engine
            in
            { return
                | model = { model | dragging = NoDrag, engine = engine }
                , mobile = { mobile | gears = gears }
                , toUndo = Do
                , toEngine = v
            }

        -- VOLUME
        ( IWheel id, Interact.Dragged { absD } _ ( True, _, _ ), NoDrag ) ->
            let
                res =
                    doVolumeChange id absD mobile model.engine
            in
            { return
                | model = { model | dragging = VolumeChange }
                , mobile = res.mobile
                , toUndo = res.toUndo
                , toEngine = res.toEngine
            }

        ( IWheel id, Interact.Dragged { absD } _ _, VolumeChange ) ->
            let
                res =
                    doVolumeChange id absD mobile model.engine
            in
            { return | mobile = res.mobile, toUndo = res.toUndo, toEngine = res.toEngine }

        ( _, Interact.DragEnded _, VolumeChange ) ->
            { return | model = { model | dragging = NoDrag }, toUndo = Do }

        -- LINK -> MOTOR
        ( IWheel ( _, [] ), Interact.Dragged _ ZSurface _, CompleteLink _ ) ->
            -- If ConpleteLink, don’t move
            return

        ( IWheel ( id, [] ), Interact.Dragged { newPos } ZSurface _, _ ) ->
            { return | model = { model | dragging = HalfLink ( id, newPos ) } }

        ( IWheel ( to, [] ), Interact.DragIn, HalfLink ( from, _ ) ) ->
            { return | model = { model | dragging = CompleteLink ( from, to ) } }

        ( IWheel ( to, [] ), Interact.DragOut, CompleteLink ( from, _ ) ) ->
            let
                toCenter =
                    (Coll.get to mobile.gears).pos
            in
            { return | model = { model | dragging = HalfLink ( from, toCenter ) } }

        ( IWheel ( _, [] ), Interact.DragEnded True, CompleteLink l ) ->
            let
                ( gears, toPlay ) =
                    Motor.add l mobile.gears <| Engine.playingIds model.engine

                ( engine, v ) =
                    Engine.addPlaying toPlay mobile.gears model.engine
            in
            { return
                | model = { model | dragging = NoDrag, engine = engine }
                , mobile = { mobile | gears = gears }
                , toUndo = Do
                , toEngine = v
            }

        -- CLEAN DRAG
        ( _, Interact.DragEnded _, _ ) ->
            { return | model = { model | dragging = NoDrag } }

        _ ->
            return


interactHarmonize : Interact.Event Interactable Zone -> Model -> Mobeel -> Return
interactHarmonize event model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = []
            , outMsg = Nothing
            , cmd = Cmd.none
            }
    in
    case ( event.item, event.action, model.dragging ) of
        -- COPY
        ( IWheel ( id, [] ), Interact.Clicked _, _ ) ->
            update (CopyGear True id) ( model, mobile )

        -- RESIZE
        ( IResizeHandle id add, Interact.Dragged { startD } _ _, NoDrag ) ->
            { return
                | model = { model | dragging = SizeChange }
                , mobile = doResize id startD add mobile
                , toUndo = Group
            }

        ( IResizeHandle id add, Interact.Dragged { startD } _ _, SizeChange ) ->
            { return | mobile = doResize id startD add mobile, toUndo = Group }

        ( _, Interact.DragEnded _, SizeChange ) ->
            { return | model = { model | dragging = NoDrag }, toUndo = Do }

        -- LINK -> HARMO
        ( IWheel ( _, [] ), Interact.Dragged _ ZSurface _, CompleteLink _ ) ->
            -- If Complete Link, don’t move
            return

        ( IWheel ( id, [] ), Interact.Dragged { newPos } ZSurface _, _ ) ->
            { return | model = { model | dragging = HalfLink ( id, newPos ) } }

        ( IWheel ( to, [] ), Interact.DragIn, HalfLink ( from, _ ) ) ->
            { return | model = { model | dragging = CompleteLink ( from, to ) } }

        ( IWheel ( to, [] ), Interact.DragOut, CompleteLink ( from, _ ) ) ->
            let
                toCenter =
                    (Coll.get to mobile.gears).pos
            in
            { return | model = { model | dragging = HalfLink ( from, toCenter ) } }

        ( IWheel ( _, [] ), Interact.DragEnded True, CompleteLink l ) ->
            let
                ( newGears, mayFract ) =
                    doLinked l mobile.gears
            in
            { return
                | model =
                    { model
                        | dragging = NoDrag
                        , link =
                            Just
                                { link = l
                                , fractInput =
                                    Maybe.withDefault (TextInput "") <|
                                        Maybe.map (\f -> FractionInput f True True) mayFract
                                }
                    }
                , mobile = { mobile | gears = newGears }
                , toUndo = Do
            }

        -- TODO if doLinked returns gears unchanged, empty undo
        -- CLEAN DRAG
        ( _, Interact.DragEnded _, _ ) ->
            { return | model = { model | dragging = NoDrag } }

        _ ->
            return


interactEdit :
    Interact.Event Interactable Zone
    -> Model
    -> Mobeel
    -> Maybe { newModel : Model, newMobile : Mobeel, toUndo : ToUndo, cmd : Cmd Msg }
interactEdit event model mobile =
    let
        return =
            { newModel = model
            , newMobile = mobile
            , toUndo = NOOP
            , cmd = Cmd.none
            }
    in
    case ( event.item, event.action, model.dragging ) of
        -- SIMPLE CLIC
        ( IWheel ( id, _ ), Interact.Clicked ( _, False, False ), _ ) ->
            if model.edit == [ id ] then
                Just { return | newModel = { model | edit = [] } }

            else
                case Wheel.getContent <| Coll.get id mobile.gears of
                    Content.S s ->
                        let
                            ( wave, cmd ) =
                                Waveform.update (Waveform.ChgSound <| Sound.getPath s) model.wave
                        in
                        Just { return | newModel = { model | edit = [ id ], wave = wave }, cmd = Cmd.map WaveMsg cmd }

                    Content.C c ->
                        case c.oneSound of
                            Just one ->
                                let
                                    ( wave, cmd ) =
                                        Waveform.update (Waveform.ChgSound one.path) model.wave
                                in
                                Just
                                    { return
                                        | newModel = { model | edit = [ id ], beadCursor = 0, wave = wave }
                                        , cmd = Cmd.map WaveMsg cmd
                                    }

                            Nothing ->
                                Just { return | newModel = { model | edit = [ id ], beadCursor = 0 } }

                    _ ->
                        Just { return | newModel = { model | edit = [ id ] } }

        -- CTRL/CMD/SHIFT CLIC
        ( IWheel ( id, _ ), Interact.Clicked _, _ ) ->
            let
                already =
                    List.foldl (\el -> (||) <| el == id) False model.edit
            in
            Just
                { return
                    | newModel =
                        { model
                            | edit =
                                if already then
                                    List.filter ((/=) id) model.edit

                                else
                                    id :: model.edit
                        }
                }

        -- WEAVE
        ( ISurface, Interact.Dragged { oldPos, newPos } ZSurface _, NoDrag ) ->
            Just
                { return
                    | newModel =
                        { model
                            | dragging =
                                WeaveBeads ( oldPos, newPos ) <|
                                    computeTouch ( oldPos, newPos ) mobile.gears
                        }
                }

        ( ISurface, Interact.Dragged { newPos } ZSurface _, WeaveBeads ( p1, _ ) _ ) ->
            Just
                { return
                    | newModel =
                        { model
                            | dragging =
                                WeaveBeads ( p1, newPos ) <|
                                    computeTouch ( p1, newPos ) mobile.gears
                        }
                }

        ( ISurface, Interact.DragEnded True, WeaveBeads _ ids ) ->
            let
                ( beads, positions ) =
                    List.unzip <|
                        List.map
                            (\g ->
                                ( { length = Harmo.getLength CommonData.getWheeledContentLength g mobile.gears
                                  , wheel = g.wheel
                                  }
                                , g.pos
                                )
                            )
                        <|
                            List.sortBy (Vec.getX << .pos) <|
                                List.map (\id -> Coll.get id mobile.gears) ids
            in
            case beads of
                [] ->
                    Nothing

                head :: tail ->
                    let
                        newGear =
                            Mobile.gearFromContent (Content.C <| Collar.fromBeads head tail) <|
                                Vec.scale (1 / (toFloat <| List.length ids)) <|
                                    List.foldl Vec.add (vec2 0 0) positions
                    in
                    Just
                        { return
                            | newModel = { model | dragging = NoDrag }
                            , newMobile = { mobile | gears = Coll.insert newGear mobile.gears }
                            , toUndo = Do
                        }

        _ ->
            Nothing


interactMove :
    Interact.Event Interactable Zone
    -> Model
    -> Mobeel
    -> Maybe Return
interactMove event model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , cmd = Cmd.none
            , toEngine = []
            , outMsg = Nothing
            }
    in
    case ( event.item, event.action, model.dragging ) of
        ( IWheel ( id, [] ), Interact.Dragged { newPos } ZSurface _, _ ) ->
            let
                gearUp =
                    Gear.update <| Gear.NewPos newPos

                ( wave, cmd ) =
                    Waveform.update Waveform.CancelSel model.wave
            in
            Just
                { return
                    | model =
                        { model
                            | dragging = Moving
                            , pack = Pack.update (Pack.DragTo Nothing) model.pack
                            , wave = wave
                        }
                    , mobile = { mobile | gears = Coll.update id gearUp mobile.gears }
                    , toUndo = Group
                    , cmd = Cmd.map WaveMsg cmd
                }

        ( _, Interact.DragEnded _, Moving ) ->
            Just { return | model = { model | dragging = NoDrag }, mobile = mobile, toUndo = Do }

        ( IWheel ( id, [] ), Interact.Dragged { newPos } ZPack _, _ ) ->
            Just
                { return
                    | toUndo = Cancel
                    , model =
                        { model
                            | dragging = Packing
                            , pack =
                                Pack.update
                                    (Pack.DragTo <|
                                        Just
                                            { pos = newPos
                                            , length = Mobile.getLengthId id mobile.gears
                                            , wheel = (Coll.get id mobile.gears).wheel
                                            }
                                    )
                                    model.pack
                        }
                }

        ( IWheel ( id, [] ), Interact.DragEnded True, Packing ) ->
            Just
                { return
                    | model = { model | dragging = NoDrag, pack = Pack.update Pack.PackIt model.pack }
                }

        ( IWheel _, Interact.Dragged { absD } ZWave _, Waving ) ->
            let
                ( wave, cmd ) =
                    Waveform.update (Waveform.MoveSel <| Vec.getX absD) model.wave
            in
            Just
                { return
                    | toUndo = Cancel
                    , model = { model | wave = wave }
                    , cmd = Cmd.map WaveMsg cmd
                }

        ( IWheel ( id, [] ), Interact.Dragged { oldPos } ZWave _, _ ) ->
            case model.edit of
                [ waveId ] ->
                    let
                        waveG =
                            Coll.get waveId mobile.gears

                        ( start, end ) =
                            Wheel.getLoopPercents waveG

                        selPercentLength =
                            Mobile.getLengthId id mobile.gears * (end - start) / Mobile.getLength waveG mobile.gears

                        ( wave, cmd ) =
                            Waveform.update (Waveform.Select ( Vec.getX oldPos, selPercentLength )) model.wave
                    in
                    Just
                        { return
                            | toUndo = Cancel
                            , model =
                                { model
                                    | dragging = Waving
                                    , wave = wave
                                }
                            , cmd = Cmd.map WaveMsg cmd
                        }

                _ ->
                    Nothing

        ( IWheel ( id, [] ), Interact.DragEnded True, Waving ) ->
            case model.edit of
                [ waveId ] ->
                    let
                        waveG =
                            Coll.get waveId mobile.gears

                        waveW =
                            waveG.wheel

                        mayLoop =
                            Waveform.getSelPercents model.wave
                    in
                    case ( mayLoop, Wheel.getWheelContent waveW ) of
                        ( Just ( start, end ), Content.S s ) ->
                            let
                                ( wave, cmd ) =
                                    Waveform.update Waveform.CancelSel model.wave

                                ret =
                                    doChangeContent
                                        id
                                        (Content.S <| Sound.setLoop ( Just start, Just end ) s)
                                        Nothing
                                        model
                                        mobile

                                newModel =
                                    ret.model

                                newMobile =
                                    ret.mobile
                            in
                            Just
                                { ret
                                    | model = { newModel | dragging = NoDrag, wave = wave, edit = [ id ] }
                                    , mobile = CommonData.updateWheel ( id, [] ) (Wheel.ChangeStart start) newMobile
                                    , cmd = Cmd.batch [ ret.cmd, Cmd.map WaveMsg cmd ]
                                }

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


type InteractWaveReturn
    = ReturnWheel Wheel.Msg
    | ReturnWave Waveform.Msg


interactWave : Geer -> Interact.Event Interactable Zone -> Model -> Mobeel -> Maybe InteractWaveReturn
interactWave g event model mobile =
    let
        move part =
            case part of
                Main ->
                    mainMove

                Mini ->
                    miniMove

        mainMove d val =
            val + (Waveform.pxToSoundDist model.wave <| round <| Vec.getX d)

        miniMove d val =
            val + (Waveform.mapPxToSoundPercent model.wave <| round <| Vec.getX d)
    in
    case ( event.item, event.action ) of
        ( IWaveCursor cur, Interact.Dragged { absD } _ _ ) ->
            case cur of
                LoopEnd part ->
                    Just <|
                        ReturnWheel <|
                            Wheel.ChangeLoop
                                ( Nothing
                                , Just <| move part absD <| Tuple.second <| Wheel.getLoopPercents g
                                )

                LoopStart part ->
                    Just <|
                        ReturnWheel <|
                            Wheel.ChangeLoop
                                ( Just <| move part absD <| Tuple.first <| Wheel.getLoopPercents g
                                , Nothing
                                )

                StartOffset part ->
                    Just <| ReturnWheel <| Wheel.ChangeStart <| move part absD <| g.wheel.startPercent

                Divide i part ->
                    let
                        mayCollar =
                            case Wheel.getContent g of
                                Content.C collar ->
                                    Just collar

                                _ ->
                                    Nothing

                        mayDivs =
                            Maybe.map .divs <| Maybe.andThen .oneSound mayCollar

                        mayPercent =
                            Maybe.andThen (List.head << List.drop i) mayDivs
                    in
                    mayPercent
                        |> Maybe.map
                            (\percent ->
                                ReturnWheel <|
                                    Wheel.ChangeDiv i <|
                                        move part absD percent
                            )

                ViewStart ->
                    Just <| ReturnWave <| Waveform.MoveStartPercent <| round <| Vec.getX absD

                ViewEnd ->
                    Just <| ReturnWave <| Waveform.MoveEndPercent <| round <| Vec.getX absD

        ( IWaveSel, Interact.Dragged { absD } _ _ ) ->
            let
                mv =
                    Just << mainMove absD
            in
            Just <| ReturnWheel <| Wheel.ChangeLoop <| Tuple.mapBoth mv mv <| Wheel.getLoopPercents g

        ( IWaveMapSel, Interact.Dragged { absD } _ _ ) ->
            Just <| ReturnWave <| Waveform.MoveView <| round <| Vec.getX absD

        _ ->
            Nothing


updateAllMuteToEngine : Model -> Mobeel -> List E.Value
updateAllMuteToEngine model mobile =
    List.concatMap (\( idd, g ) -> Engine.muted ( idd, [] ) g.wheel.mute model.engine) <|
        Coll.toList mobile.gears


colorGen : Random.Generator Float
colorGen =
    Random.float 0 1


colorToString : Color.Color -> String
colorToString c =
    let
        { red, blue, green } =
            Color.toRgba c
    in
    "#" ++ floatToHex red ++ floatToHex green ++ floatToHex blue


hexDigits : List String
hexDigits =
    [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f" ]


intToHex : Int -> String
intToHex i =
    Maybe.withDefault "0" <| Dict.get i <| Dict.fromList <| List.indexedMap Tuple.pair hexDigits


floatToHex : Float -> String
floatToHex f =
    let
        i =
            round (f * 255)
    in
    intToHex (i // 16) ++ intToHex (modBy 16 i)


hexToInt : String -> Int
hexToInt s =
    Maybe.withDefault 0 <| Dict.get s <| Dict.fromList <| List.indexedMap (\i c -> ( c, i )) hexDigits


hexToFloat : String -> Float
hexToFloat s =
    toFloat (hexToInt (String.slice 0 1 s) * 16 + (hexToInt <| String.slice 1 2 s)) / 255


type IntOrEmpty
    = Int Int
    | Empty


toIntOrEmpty : String -> Maybe IntOrEmpty
toIntOrEmpty str =
    if String.isEmpty str then
        Just Empty

    else
        Maybe.map Int <| String.toInt str
