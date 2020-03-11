port module Editor.Mobile exposing (..)

import Coll exposing (Coll, Id)
import Color
import Data.Collar as Collar
import Data.Common as CommonData exposing (Identifier)
import Data.Content as Content exposing (Content)
import Data.Gear as Gear
import Data.Mobile as Mobile exposing (Geer, Mobeel)
import Data.Wheel as Wheel exposing (Conteet, Wheel)
import Dict
import Editor.Interacting exposing (..)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
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
import Link exposing (Link)
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



-- TODO Maybe delegate Coll dependence to Data.Mobile (except Id)


svgId : String
svgId =
    "svg"


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
    , cursor : Int
    , link : Maybe LinkInfo
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


keyCodeToMode : List ( String, Mode )
keyCodeToMode =
    [ ( "KeyD", Move )
    , ( "KeyV", Nav )
    , ( "Delete", SupprMode )
    , ( "Backspace", SupprMode )
    , ( "KeyQ", Alternate )
    , ( "KeyS", Solo )
    ]


type alias LinkInfo =
    { link : Link Geer, fractInput : FractInput }


type FractInput
    = FractionInput Fraction
    | TextInput String


type Dragging
    = NoDrag
    | HalfLink ( Id Geer, Vec2 )
    | CompleteLink (Link Geer)
    | Cut ( Vec2, Vec2 ) (List (Link Geer))
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
    , cursor = 0
    , link = Nothing
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
            Maybe.withDefault (PanSvg.init svgId) <|
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
    | CopyGear (Id Geer)
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
    | Collared (Id Geer)
    | UnCollar (Id Geer)
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
    | Writing Bool


type ToUndo
    = Do
    | Group
    | Cancel
    | NOOP


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
                    Debug.log (D.errorToString err) return

        PlayGear ->
            case model.tool of
                Edit _ ->
                    let
                        ( engine, v ) =
                            Engine.addPlaying model.edit mobile.gears model.engine
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
                            { return | model = { model | cursor = min (model.cursor + 1) <| Collar.length col } }

                        _ ->
                            return

                _ ->
                    return

        CursorLeft ->
            case model.edit of
                [ id ] ->
                    case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                        Content.C _ ->
                            { return | model = { model | cursor = max (model.cursor - 1) 0 } }

                        _ ->
                            return

                _ ->
                    return

        NewBead c ->
            case model.edit of
                [ id ] ->
                    case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                        Content.C col ->
                            { return
                                | mobile =
                                    CommonData.updateWheel ( id, [] )
                                        (Wheel.ChangeContent <| Content.C <| Collar.add model.cursor (Collar.beadFromContent c) col)
                                        mobile
                                , toUndo = Group
                                , model = { model | cursor = model.cursor + 1 }
                                , cmd = Random.generate (\color -> WheelMsgs [ ( ( id, [ model.cursor ] ), Wheel.ChangeColor color ) ]) colorGen
                            }

                        _ ->
                            return

                _ ->
                    return

        UnpackBead ( w, l ) new ->
            case model.edit of
                [ id ] ->
                    case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                        Content.C col ->
                            if new then
                                { return
                                    | mobile =
                                        CommonData.updateWheel ( id, [] )
                                            (Wheel.ChangeContent <| Content.C <| Collar.add model.cursor { wheel = w, length = l } col)
                                            mobile
                                    , toUndo = Do
                                    , model = { model | cursor = model.cursor + 1 }
                                }

                            else
                                Debug.todo "ChangeContent of bead, has to select bead"

                        {- case model.common.edit of
                           [ B i ] ->
                               update (WheelMsg ( i, Wheel.ChangeContent <| Wheel.getContent { wheel = w } )) ( model, collar )

                           _ ->
                               return
                        -}
                        _ ->
                            return

                _ ->
                    return

        CopyGear id ->
            { return | mobile = { mobile | gears = Gear.copy id mobile.gears }, toUndo = Do }

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
            { return
                | model =
                    { model
                        | edit =
                            if l == [] && id /= mobile.motor then
                                List.filter ((/=) id) model.edit

                            else
                                model.edit
                        , cursor =
                            if model.edit == [ id ] then
                                case l of
                                    [ i ] ->
                                        case Wheel.getWheelContent <| CommonData.getWheel ( id, [] ) mobile of
                                            Content.C _ ->
                                                if model.cursor > i then
                                                    model.cursor - 1

                                                else
                                                    model.cursor

                                            _ ->
                                                model.cursor

                                    _ ->
                                        model.cursor

                            else
                                model.cursor
                        , engine = Engine.init
                    }
                , toUndo = Do
                , toEngine = [ Engine.stop ]
                , mobile = CommonData.deleteWheel ( id, l ) mobile Mobile.rm Collar.rm
            }

        EnteredFract isNumerator str ->
            Maybe.map2 Tuple.pair model.link (String.toInt str)
                |> Maybe.map
                    (\( link, i ) ->
                        { return
                            | model =
                                { model
                                    | link =
                                        Just
                                            { link
                                                | fractInput =
                                                    case link.fractInput of
                                                        FractionInput fract ->
                                                            if isNumerator then
                                                                FractionInput { fract | num = i }

                                                            else
                                                                FractionInput { fract | den = i }

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
            case model.link of
                Nothing ->
                    return

                Just link ->
                    case link.fractInput of
                        FractionInput _ ->
                            return

                        TextInput _ ->
                            { return | model = { model | link = Just { link | fractInput = TextInput str } } }

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
                            { model | link = Just { link | fractInput = FractionInput fract } }

                        Nothing ->
                            model
            }

        SimplifyFractView ->
            model.link
                |> Maybe.map
                    (\link ->
                        case link.fractInput of
                            FractionInput fract ->
                                { return
                                    | model =
                                        { model
                                            | link =
                                                Just
                                                    { link
                                                        | fractInput = FractionInput <| Fract.simplify fract
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
                            Harmo.changeSelf id
                                (CommonData.getContentLength <| Wheel.getContent <| Coll.get id mobile.gears)
                                mobile.gears
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
                    { m | motor = Motor.default, harmony = Harmo.newSelf <| Harmo.getLength m.harmony mobile.gears }

                subMobile =
                    List.foldl
                        (\i acc ->
                            let
                                g =
                                    Coll.get i mobile.gears

                                newG =
                                    { g
                                        | motor = Motor.default
                                        , harmony = Harmo.newSelf <| Harmo.getLength g.harmony mobile.gears
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

        Collared id ->
            let
                g =
                    Coll.get id mobile.gears

                collar =
                    Collar.fromWheel g.wheel <| Harmo.getLength g.harmony mobile.gears
            in
            { return
                | mobile = { mobile | gears = Coll.update id (Wheel.setContent <| Content.C collar) mobile.gears }
                , toUndo = Do
            }

        UnCollar id ->
            let
                g =
                    Coll.get id mobile.gears
            in
            case Wheel.getContent g of
                Content.C col ->
                    { return
                        | mobile = Mobile.updateGear id (Wheel.setContent <| Wheel.getContent col.head) mobile
                        , toUndo = Do
                    }

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
                    Debug.log (D.errorToString e) return

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
                                            if startZone == ZWave || dragZone == ZWave then
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
    PanSvg.newSVGSize (SVGSize << D.decodeValue PanSvg.sizeDecoder)
        :: Sub.map WaveMsg Waveform.sub
        :: (gotRecord <| (GotRecord << D.decodeValue D.string))
        :: (List.map (Sub.map InteractMsg) <| Interact.subs interact)
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



-- TODO Split between mobile view, motor view, harmony view, and whatever else


viewContent : ( Model, Mobeel ) -> Element Msg
viewContent ( model, mobile ) =
    let
        ( wavePoints, viewWave ) =
            case model.edit of
                [ id ] ->
                    let
                        g =
                            Coll.get id mobile.gears

                        ( start, end ) =
                            Wheel.getLoopPercents g
                    in
                    ( { offset = g.wheel.startPercent, start = start, end = end }
                    , case ( model.tool, Wheel.getContent g ) of
                        ( Edit _, Content.S s ) ->
                            (model.wave.drawn == (Waveform.SoundDrawn <| Sound.toString s))
                                && g.wheel.viewContent

                        _ ->
                            False
                    )

                _ ->
                    ( { offset = 0, start = 0, end = 0 }, False )

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
                viewWave
                model.wave
                wavePoints
                model.interact
                InteractMsg
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
                                (Harmo.getLength g.harmony mobile.gears)
                                { mod = getMod id
                                , motor = id == mobile.motor
                                , dashed = Harmo.hasHarmonics g.harmony
                                , baseColor =
                                    Maybe.map (\bId -> (Coll.get bId mobile.gears).wheel.color) <|
                                        Harmo.getBaseId g.harmony
                                , named =
                                    case Interact.getInteract model.interact of
                                        Just ( IWheel idd, _ ) ->
                                            id == Tuple.first idd

                                        _ ->
                                            False
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
                                    let
                                        g =
                                            Coll.get id mobile.gears
                                    in
                                    case model.tool of
                                        Play _ _ ->
                                            let
                                                length =
                                                    Harmo.getLength g.harmony mobile.gears
                                            in
                                            [ Link.drawMotorLink ( ( g.pos, length ), ( pos, length ) ) ]

                                        Harmonize ->
                                            [ Link.drawRawLink
                                                ( g.pos, pos )
                                                (Harmo.getLength g.harmony mobile.gears)
                                                Link.baseColor
                                            ]
                                                ++ [ S.g [ SA.opacity <| Opacity 0 ] <|
                                                        List.map
                                                            (\( idd, gg ) ->
                                                                Wheel.view gg.wheel
                                                                    gg.pos
                                                                    (Harmo.getLength gg.harmony mobile.gears)
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
                                            Link.viewMotorLink False <| Gear.toDrawLink mobile.gears l

                                        Harmonize ->
                                            Link.viewFractLink (Gear.toDrawLink mobile.gears l) <| ILink l

                                        _ ->
                                            []

                                Cut seg _ ->
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
                                                Gear.toDrawLink mobile.gears l
                                        )
                                    <|
                                        Motor.getAllLinks mobile.gears

                                Harmonize ->
                                    (case Interact.getInteract model.interact of
                                        Just ( ILink l, _ ) ->
                                            Link.viewFractOnLink (Gear.toDrawLink mobile.gears l) <|
                                                Fract.simplify <|
                                                    Fract.division
                                                        (Coll.get (Tuple.second l) mobile.gears).harmony.fract
                                                        (Coll.get (Tuple.first l) mobile.gears).harmony.fract

                                        _ ->
                                            []
                                    )
                                        ++ (List.concatMap (\l -> Link.viewFractLink (Gear.toDrawLink mobile.gears l) (ILink l)) <|
                                                List.concatMap (.harmony >> Harmo.getLinks) <|
                                                    Coll.values mobile.gears
                                           )
                                        ++ (case model.link of
                                                Just { link, fractInput } ->
                                                    Link.viewSelectedLink (Gear.toDrawLink mobile.gears link) <|
                                                        case fractInput of
                                                            FractionInput _ ->
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
                                                    Harmo.getLength g.harmony mobile.gears

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
                                                        , SA.x <| Num <| Collar.getCumulLengthAt model.cursor col - cursorW / 2
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



-- TODO duplicate with Gear.getName and Common.getName


getNameWithDefault : Id Geer -> Mobeel -> String
getNameWithDefault id mobile =
    let
        w =
            (Coll.get id mobile.gears).wheel
    in
    if String.isEmpty w.name then
        Gear.toUID id

    else
        w.name



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
                [ text <| getNameWithDefault id mobile
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
                [ Input.text ([ Font.color (rgb 0 0 0) ] ++ updateWriting)
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
                                        "Ranger " ++ Sound.toString s

                                    else
                                        "Voir " ++ Sound.toString s
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
                    , case Wheel.getContent g of
                        Content.C col ->
                            if List.length col.beads == 0 then
                                Input.button []
                                    { label = text "Décollier"
                                    , onPress = Just <| UnCollar id
                                    }

                            else
                                Input.button []
                                    { label = text "Collier"
                                    , onPress = Just <| Collared id
                                    }

                        _ ->
                            Input.button []
                                { label = text "Collier"
                                , onPress = Just <| Collared id
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
                                getNameWithDefault rId mobile
                            )
                , text <| "( " ++ (Round.round 2 <| Harmo.getLengthId id mobile.gears) ++ " )"
                , text <|
                    "Contenu : "
                        ++ (Round.round 2 <| CommonData.getContentLength <| Wheel.getContent g)
                ]
            ]

        _ :: _ ->
            [ viewDetailsColumn (rgb 0.5 0.5 0.5) <|
                (List.map (\id -> text <| getNameWithDefault id mobile) <| List.reverse model.edit)
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
                            FractionInput fract ->
                                [ Input.text ([ Font.color (rgb 0 0 0) ] ++ updateWriting)
                                    { text = String.fromInt fract.num
                                    , onChange = EnteredFract True
                                    , label = Input.labelHidden "Numerator"
                                    , placeholder = Nothing
                                    }
                                , text "/"
                                , Input.text ([ Font.color (rgb 0 0 0) ] ++ updateWriting)
                                    { text = String.fromInt fract.den
                                    , onChange = EnteredFract False
                                    , label = Input.labelHidden "Denominator"
                                    , placeholder = Nothing
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
                                [ Input.text ([ Font.color (rgb 0 0 0) ] ++ updateWriting)
                                    { placeholder =
                                        Just <|
                                            Input.placeholder [] <|
                                                text <|
                                                    Round.round
                                                        5
                                                    <|
                                                        Harmo.getLengthId (Tuple.second link) mobile.gears
                                                            / Harmo.getLengthId (Tuple.first link) mobile.gears
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
            Harmo.getLengthId id gears

        dd =
            if add then
                Vec.getX d

            else
                -1 * Vec.getX d

        newSize =
            abs <| dd * 2 + length
    in
    { mobile | gears = Harmo.resizeFree id newSize gears }


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

        gears =
            List.foldl (\el -> Coll.update el chSound) mobile.gears group

        newModel =
            -- TODO Why !!??
            { model | mode = Normal }
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
                , model = newModel
            }

        Nothing ->
            let
                colorToMsgs color =
                    List.map (\el -> ( ( el, [] ), Wheel.ChangeColor color )) group
            in
            { return
                | mobile = { mobile | gears = gears }
                , toUndo = Group
                , model = newModel
                , cmd = Random.generate (WheelMsgs << colorToMsgs) colorGen
            }


computeCuts : ( Vec2, Vec2 ) -> Coll Geer -> List (Link Geer)
computeCuts cut gears =
    Motor.getAllLinks gears
        |> List.filter (Link.cuts cut << Link.toSegment << Gear.toDrawLink gears)



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
                    -- TODO Should change mode to Normal here instead of if doChangeContent?
                    doChangeContent id (Content.S s) Nothing model mobile

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
                                    case interactSelectEdit event mobile model of
                                        Just ( newModel, cmd ) ->
                                            let
                                                ret =
                                                    update StopGear ( newModel, mobile )
                                            in
                                            { ret | cmd = Cmd.batch [ cmd, ret.cmd ] }

                                        Nothing ->
                                            case model.edit of
                                                [ id ] ->
                                                    let
                                                        g =
                                                            Coll.get id mobile.gears
                                                    in
                                                    case interactWave g event model mobile of
                                                        Just subMsg ->
                                                            update (WheelMsgs [ ( ( id, [] ), subMsg ) ]) ( model, mobile )

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
            { return | model = { model | dragging = Cut ( oldPos, newPos ) <| computeCuts ( oldPos, newPos ) mobile.gears } }

        ( ISurface, Interact.Dragged { newPos } ZSurface _, Cut ( p1, _ ) _ ) ->
            { return | model = { model | dragging = Cut ( p1, newPos ) <| computeCuts ( p1, newPos ) mobile.gears } }

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
            { return | mobile = { mobile | gears = Gear.copy id mobile.gears }, toUndo = Do }

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
                                , fractInput = Maybe.withDefault (TextInput "") <| Maybe.map FractionInput mayFract
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


interactSelectEdit : Interact.Event Interactable Zone -> Mobeel -> Model -> Maybe ( Model, Cmd Msg )
interactSelectEdit event mobile model =
    case ( event.item, event.action ) of
        ( IWheel ( id, _ ), Interact.Clicked ( _, False, False ) ) ->
            if model.edit == [ id ] then
                Just ( { model | edit = [] }, Cmd.none )

            else
                case Wheel.getContent <| Coll.get id mobile.gears of
                    Content.S s ->
                        let
                            ( wave, cmd ) =
                                Waveform.update (Waveform.ChgSound <| Sound.toString s) model.wave
                        in
                        Just ( { model | edit = [ id ], wave = wave }, Cmd.map WaveMsg cmd )

                    Content.C _ ->
                        Just ( { model | edit = [ id ], cursor = 0 }, Cmd.none )

                    _ ->
                        Just ( { model | edit = [ id ] }, Cmd.none )

        ( IWheel ( id, _ ), Interact.Clicked _ ) ->
            let
                already =
                    List.foldl (\el -> (||) <| el == id) False model.edit
            in
            Just
                ( { model
                    | edit =
                        if already then
                            List.filter ((/=) id) model.edit

                        else
                            id :: model.edit
                  }
                , Cmd.none
                )

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
                                            , length = Harmo.getLengthId id mobile.gears
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
                            Harmo.getLengthId id mobile.gears * (end - start) / Harmo.getLength waveG.harmony mobile.gears

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


interactWave : Geer -> Interact.Event Interactable Zone -> Model -> Mobeel -> Maybe Wheel.Msg
interactWave g event model mobile =
    let
        move d val =
            val + (Vec.getX d / toFloat model.wave.size)
    in
    case ( event.item, event.action ) of
        ( IWaveCursor cur, Interact.Dragged { absD } _ _ ) ->
            case cur of
                LoopEnd ->
                    Just <|
                        Wheel.ChangeLoop
                            ( Nothing
                            , Just <| move absD <| Tuple.second <| Wheel.getLoopPercents g
                            )

                LoopStart ->
                    Just <|
                        Wheel.ChangeLoop
                            ( Just <| move absD <| Tuple.first <| Wheel.getLoopPercents g
                            , Nothing
                            )

                StartOffset ->
                    Just <| Wheel.ChangeStart <| move absD <| g.wheel.startPercent

        ( IWaveSel, Interact.Dragged { absD } _ _ ) ->
            let
                mv =
                    Just << move absD
            in
            Just <| Wheel.ChangeLoop <| Tuple.mapBoth mv mv <| Wheel.getLoopPercents g

        _ ->
            Nothing


updateAllMuteToEngine : Model -> Mobeel -> List E.Value
updateAllMuteToEngine model mobile =
    List.concatMap (\( idd, g ) -> Engine.muted ( idd, [] ) g.wheel.mute model.engine) <|
        Coll.toList mobile.gears


updateWriting : List (Attribute Msg)
updateWriting =
    [ Events.onFocus <| OutMsg <| Writing True, Events.onLoseFocus <| OutMsg <| Writing False ]


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
