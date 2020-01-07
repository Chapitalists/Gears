module Editor.Mobile exposing (..)

import Coll exposing (Coll, Id)
import Color
import Data.Collar as Collar
import Data.Content as Content exposing (Content)
import Data.Gear as Gear
import Data.Mobile as Mobile exposing (Geer, Mobeel)
import Data.Wheel as Wheel exposing (Wheel)
import Editor.Common exposing (..)
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Engine exposing (Engine)
import Fraction as Fract exposing (Fraction)
import Harmony as Harmo
import Html.Attributes
import Interact exposing (Interact)
import Json.Encode as E
import Link exposing (Link)
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Motor
import PanSvg
import Random
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Core as Svg exposing (Svg)


type alias Model =
    { dragging : Dragging
    , tool : Tool
    , mode : Mode
    , link : Maybe LinkInfo
    , engine : Engine
    , interact : Interact.State Interactable
    , common : CommonModel
    , svg : PanSvg.Model
    }



-- TODO Mix Dragging inside Tool to make impossible states impossible ?


type Tool
    = Edit
    | Play Bool
    | Harmonize


type Mode
    = CommonMode CommonMode
    | Move
    | SelectMotor
    | ChangeSound (Id Geer)


keyCodeToMode : List ( String, Mode )
keyCodeToMode =
    [ ( "KeyD", Move )
    ]


type alias LinkInfo =
    { link : Link Geer, fract : Maybe Fraction }


type Dragging
    = NoDrag
    | HalfLink ( Id Geer, Vec2 )
    | CompleteLink (Link Geer)
    | Cut ( Vec2, Vec2 ) (List (Link Geer))
    | VolumeChange
    | SizeChange
    | Moving


init : Maybe Mobeel -> Maybe PanSvg.Model -> Model
init may svg =
    { dragging = NoDrag
    , tool = Play False
    , mode = CommonMode Normal
    , link = Nothing
    , engine = Engine.init
    , interact = Interact.init
    , common = commonInit
    , svg =
        let
            base =
                Maybe.withDefault PanSvg.init svg
        in
        Maybe.withDefault base <| Maybe.map (\m -> PanSvg.centerZoom (Mobile.gearPosSize m.motor m.gears) base) may
    }


type Msg
    = ChangedTool Tool
    | ChangedMode Mode
      -- TODO EngineMsg ?
    | ToggleEngine
    | PlayGear (Id Geer)
    | StopGear (Id Geer)
      --
    | SoundClicked Sound
    | CopyGear (Id Geer)
    | NewGear (Content Wheel)
    | DeleteGear (Id Geer)
    | EnteredFract Bool String -- True for Numerator
    | AppliedFract (Link Geer) Fraction
    | SimplifyFractView
    | Capsuled (Id Geer)
    | Collared (Id Geer)
    | InteractMsg (Interact.Msg Interactable)
    | SvgMsg PanSvg.Msg
    | WheelMsg ( Id Geer, Wheel.Msg )
    | GearMsg ( Id Geer, Gear.Msg )
    | OutMsg DocMsg


type alias Return =
    { model : Model
    , mobile : Mobeel
    , toUndo : ToUndo
    , toEngine : Maybe E.Value
    , outMsg : Maybe DocMsg
    , cmd : Cmd Msg
    }


update : Msg -> ( Model, Mobeel ) -> Return
update msg ( model, mobile ) =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = Nothing
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
                                    if tool == Edit then
                                        NoDrag

                                    else
                                        model.dragging
                        , engine = Engine.init
                    }
                , toEngine = Just Engine.stop
            }

        ChangedMode mode ->
            { return | model = { model | mode = mode } }

        ToggleEngine ->
            case model.tool of
                Play True ->
                    { return | model = { model | tool = Play False, engine = Engine.init }, toEngine = Just Engine.stop }

                Play False ->
                    let
                        ( engine, v ) =
                            Engine.addPlaying
                                (Motor.getMotored mobile.motor mobile.gears)
                                mobile.gears
                                model.engine
                    in
                    { return | model = { model | tool = Play True, engine = engine }, toEngine = v }

                _ ->
                    return

        PlayGear id ->
            let
                ( engine, v ) =
                    Engine.addPlaying [ id ] mobile.gears model.engine
            in
            { return | model = { model | engine = engine }, toEngine = v }

        StopGear id ->
            { return | model = { model | engine = Engine.init }, toEngine = Just Engine.stop }

        SoundClicked s ->
            case model.mode of
                ChangeSound id ->
                    let
                        group =
                            Harmo.getHarmonicGroup (Coll.idMap id) mobile.gears

                        chSound =
                            Wheel.update <| Wheel.ChangeContent <| Content.S s
                    in
                    { return
                        | mobile = { mobile | gears = List.foldl (\el -> Coll.update el chSound) mobile.gears group }
                        , toUndo = Do
                        , model = { model | mode = CommonMode Normal }
                    }

                _ ->
                    update (NewGear <| Content.S s) ( model, mobile )

        CopyGear id ->
            { return | mobile = { mobile | gears = Gear.copy id mobile.gears }, toUndo = Do }

        NewGear content ->
            let
                pos =
                    vec2 50 50

                ( id, gears ) =
                    Coll.insertTellId (Mobile.gearFromContent content pos) mobile.gears

                colorGen =
                    Random.map (\f -> Color.hsl f 1 0.5) <| Random.float 0 1
            in
            { return
                | mobile = { mobile | gears = gears }
                , toUndo = Do
                , model = { model | svg = PanSvg.centerZoom (Mobile.gearPosSize id gears) model.svg }
                , cmd = Random.generate (\color -> WheelMsg ( id, Wheel.ChangeColor color )) colorGen
            }

        DeleteGear id ->
            if id == mobile.motor then
                return

            else
                let
                    gears =
                        Motor.clean id mobile.gears

                    harmo =
                        (Coll.get id gears).harmony
                in
                -- TODO check and use harmo clean
                if Harmo.hasHarmonics harmo then
                    -- TODO delete base ?
                    Debug.log "TODO delete base" return

                else
                    { return
                        | model =
                            { model
                                | common = commonUpdate (Delete <| G id) model.common
                                , engine = Engine.init
                            }
                        , toUndo = Do
                        , toEngine = Just Engine.stop
                        , mobile =
                            case Harmo.getBaseId harmo of
                                Nothing ->
                                    { mobile | gears = Coll.remove id gears }

                                Just baseId ->
                                    { mobile
                                        | gears =
                                            gears
                                                |> Coll.update baseId (Harmo.remove id)
                                                |> Coll.remove id
                                    }
                    }

        EnteredFract isNumerator str ->
            Maybe.map2 Tuple.pair model.link (String.toInt str)
                |> Maybe.andThen
                    (\( link, i ) ->
                        link.fract
                            |> Maybe.map
                                (\fract ->
                                    { return
                                        | model =
                                            { model
                                                | link =
                                                    Just
                                                        { link
                                                            | fract =
                                                                Just <|
                                                                    if isNumerator then
                                                                        { fract | num = i }

                                                                    else
                                                                        { fract | den = i }
                                                        }
                                            }
                                    }
                                )
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

        SimplifyFractView ->
            model.link
                |> Maybe.andThen
                    (\link ->
                        link.fract
                            |> Maybe.map
                                (\fract ->
                                    { return | model = { model | link = Just { link | fract = Just <| Fract.simplify fract } } }
                                )
                    )
                |> Maybe.withDefault return

        Capsuled id ->
            let
                g =
                    Coll.get id mobile.gears

                newG =
                    { g | motor = Motor.default, harmony = Harmo.newSelf <| Harmo.getLength g.harmony mobile.gears }
            in
            { return
                | mobile =
                    { mobile
                        | gears =
                            Coll.update id
                                (Wheel.setContent <| Content.M <| Mobile.fromGear newG)
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

        WheelMsg ( id, subMsg ) ->
            { return | mobile = { mobile | gears = Coll.update id (Wheel.update subMsg) mobile.gears }, toUndo = Do }

        GearMsg ( id, subMsg ) ->
            { return | mobile = { mobile | gears = Coll.update id (Gear.update subMsg) mobile.gears }, toUndo = Do }

        OutMsg subMsg ->
            { return | outMsg = Just subMsg }

        SvgMsg subMsg ->
            { return | model = { model | svg = PanSvg.update subMsg model.svg } }

        -- TODO use some pattern like outMessage package? or elm-state? elm-return?
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
                    let
                        inEvent =
                            case e.action of
                                Interact.Dragged pos1 pos2 k ->
                                    { e
                                        | action =
                                            Interact.Dragged
                                                (PanSvg.mapIn pos1 newModel.svg)
                                                (PanSvg.mapIn pos2 newModel.svg)
                                                k
                                    }

                                _ ->
                                    e
                    in
                    manageInteractEvent inEvent newModel mobile


subs : Model -> List (Sub Msg)
subs { interact } =
    (Sub.map SvgMsg <| PanSvg.sub)
        :: (List.map (Sub.map InteractMsg) <| Interact.subs interact)


viewTools : Model -> Element Msg
viewTools model =
    Input.radioRow [ spacing 30 ]
        { onChange = ChangedTool
        , options =
            [ Input.option (Play False) <| text "Jeu (W)"
            , Input.option Harmonize <| text "Harmonie (X)"
            , Input.option Edit <| text "Édition (C)"
            ]
        , selected = Just model.tool
        , label = Input.labelHidden "Outils"
        }


viewExtraTools : Model -> Element Msg
viewExtraTools model =
    row [ width fill, padding 20 ]
        (case model.tool of
            Play on ->
                [ Input.button [ centerX ]
                    { label =
                        if on then
                            text "Stop"

                        else
                            text "Jouer"
                    , onPress = Just ToggleEngine
                    }
                ]

            _ ->
                []
        )



-- TODO Split between mobile view, motor view, harmony view, and whatever else


viewContent : ( Model, Mobeel ) -> Element Msg
viewContent ( model, mobile ) =
    let
        getMod : Id Geer -> Wheel.Mod
        getMod id =
            if model.tool == Edit && model.common.edit == Just (G id) then
                Wheel.Selected

            else
                case Interact.getInteract model.interact of
                    Just ( IWheel iid, mode ) ->
                        if iid /= G id then
                            Wheel.None

                        else
                            case ( model.tool, mode ) of
                                ( Harmonize, Interact.Hover ) ->
                                    Wheel.Resizing

                                ( Edit, Interact.Hover ) ->
                                    Wheel.Selectable

                                _ ->
                                    Wheel.None

                    Just ( IResizeHandle iid _, mode ) ->
                        if iid /= G id then
                            Wheel.None

                        else
                            Wheel.Resizing

                    _ ->
                        Wheel.None
    in
    Element.html <|
        S.svg
            (List.map (Html.Attributes.map SvgMsg) (PanSvg.svgAttributes model.svg)
                ++ (List.map (Html.Attributes.map InteractMsg) <|
                        Interact.dragSpaceEvents model.interact
                            ++ Interact.draggableEvents ISurface
                   )
            )
        <|
            List.map (Svg.map InteractMsg) <|
                (List.map
                    (\( id, g ) ->
                        Wheel.view g.wheel
                            g.pos
                            (Harmo.getLength g.harmony mobile.gears)
                            { mod = getMod id, motor = id == mobile.motor, dashed = Harmo.hasHarmonics g.harmony }
                            (G id)
                            (Gear.toUID id)
                            |> Svg.map (Interact.map fromWheelInteractable)
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
                                    Play _ ->
                                        let
                                            length =
                                                Harmo.getLength g.harmony mobile.gears
                                        in
                                        [ Link.drawMotorLink ( ( g.pos, length ), ( pos, length ) ) ]

                                    Harmonize ->
                                        [ Link.drawRawLink
                                            ( g.pos, pos )
                                            (Harmo.getLength g.harmony mobile.gears)
                                        ]

                                    _ ->
                                        []

                            CompleteLink l ->
                                case model.tool of
                                    Play _ ->
                                        Link.viewMotorLink False <| Gear.toDrawLink mobile.gears l

                                    Harmonize ->
                                        Link.viewFractLink <| Gear.toDrawLink mobile.gears l

                                    _ ->
                                        []

                            Cut seg _ ->
                                [ Link.drawCut seg <| PanSvg.getScale model.svg ]

                            _ ->
                                []
                       )
                    ++ (case model.tool of
                            Play _ ->
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
                                (List.concatMap (Link.viewFractLink << Gear.toDrawLink mobile.gears) <|
                                    List.concatMap (.harmony >> Harmo.getLinks) <|
                                        Coll.values mobile.gears
                                )
                                    ++ (case model.link of
                                            Just { link } ->
                                                Link.viewSelectedLink <| Gear.toDrawLink mobile.gears link

                                            _ ->
                                                []
                                       )

                            _ ->
                                []
                       )


viewDetails : Model -> Mobeel -> List (Element Msg)
viewDetails model mobile =
    case model.mode of
        ChangeSound id ->
            [ column [ height fill, Bg.color (rgb 0.5 0.2 0), Font.color (rgb 1 1 1), spacing 20, padding 10 ] <|
                [ text <| Gear.toUID id
                , text "Choisir un son chargé"
                , Input.button []
                    { label = text "Annuler"
                    , onPress = Just <| ChangedMode <| CommonMode Normal
                    }
                ]
            ]

        SelectMotor ->
            [ column [ height fill, Bg.color (rgb 0.5 0.2 0), Font.color (rgb 1 1 1), spacing 20, padding 10 ] <|
                [ text "Choisir nouvelle Motrice"
                , Input.button []
                    { label = text "Annuler"
                    , onPress = Just <| ChangedMode <| CommonMode Normal
                    }
                ]
            ]

        _ ->
            case model.tool of
                Edit ->
                    viewEditDetails model mobile

                Harmonize ->
                    viewHarmonizeDetails model mobile

                _ ->
                    []


viewEditDetails : Model -> Mobeel -> List (Element Msg)
viewEditDetails model mobile =
    case model.common.edit of
        Just (G id) ->
            let
                g =
                    Coll.get id mobile.gears
            in
            [ viewDetailsColumn
                [ viewNameInput g (Gear.toUID id) <| \str -> WheelMsg ( id, Wheel.Named str )
                , viewContentButton g <| OutMsg <| Inside <| G id
                , Input.button []
                    { label = text "PlayPause"
                    , onPress = Just <| PlayGear id
                    }
                , Input.button []
                    { label = text "Stop"
                    , onPress = Just <| StopGear id
                    }
                , viewVolumeSlider g <| \f -> WheelMsg ( id, Wheel.ChangeVolume f )
                , Input.button []
                    { label = text "Copie"
                    , onPress = Just <| CopyGear id
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
                , viewChangeContent <| ChangedMode <| ChangeSound id
                , Input.button []
                    { label = text "Encapsuler"
                    , onPress = Just <| Capsuled id
                    }
                , Input.button []
                    { label = text "Collier"
                    , onPress = Just <| Collared id
                    }
                , if id == mobile.motor then
                    Input.button []
                        { onPress = Just <| ChangedMode SelectMotor
                        , label = text "Changer Motrice"
                        }

                  else
                    viewDeleteButton <| DeleteGear id
                ]
            ]

        _ ->
            []


viewHarmonizeDetails : Model -> Mobeel -> List (Element Msg)
viewHarmonizeDetails model mobile =
    case model.link of
        Just { link, fract } ->
            [ viewDetailsColumn
                ([ text <| (Gear.toUID <| Tuple.first link) ++ (Gear.toUID <| Tuple.second link) ]
                    ++ (case fract of
                            Nothing ->
                                [ text <|
                                    String.fromFloat <|
                                        Harmo.getLengthId (Tuple.second link) mobile.gears
                                            / Harmo.getLengthId (Tuple.first link) mobile.gears
                                ]

                            Just f ->
                                [ Input.text [ Font.color (rgb 0 0 0) ]
                                    { text = String.fromInt f.num
                                    , onChange = EnteredFract True
                                    , label = Input.labelHidden "Numerator"
                                    , placeholder = Nothing
                                    }
                                , text "/"
                                , Input.text [ Font.color (rgb 0 0 0) ]
                                    { text = String.fromInt f.den
                                    , onChange = EnteredFract False
                                    , label = Input.labelHidden "Denominator"
                                    , placeholder = Nothing
                                    }
                                , Input.button []
                                    { label = text "Change"
                                    , onPress = Just <| AppliedFract link f
                                    }
                                , Input.button []
                                    { label = text "Simplifier"
                                    , onPress = Just SimplifyFractView
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


doVolumeChange : Id Geer -> Vec2 -> Vec2 -> Float -> Mobeel -> Engine -> { mobile : Mobeel, toUndo : ToUndo, toEngine : Maybe E.Value }
doVolumeChange id oldPos newPos scale mobile engine =
    let
        gears =
            mobile.gears

        wheel =
            (Coll.get id gears).wheel

        volume =
            wheel.volume + (Vec.getY oldPos - Vec.getY newPos) / scale / 100

        newGears =
            Coll.update id (Wheel.update <| Wheel.ChangeVolume volume) gears
    in
    { mobile = { mobile | gears = newGears }, toUndo = Group, toEngine = Engine.volumeChanged id volume engine }


doResize : Id Geer -> Vec2 -> Vec2 -> Bool -> Mobeel -> Mobeel
doResize id oldPos newPos add mobile =
    let
        gears =
            mobile.gears

        length =
            Harmo.getLengthId id gears

        d =
            Vec.getX newPos - Vec.getX oldPos

        dd =
            if add then
                d

            else
                -d

        newSize =
            abs <| dd * 2 + length
    in
    { mobile | gears = Harmo.resizeFree id newSize gears }


computeCuts : ( Vec2, Vec2 ) -> Coll Geer -> List (Link Geer)
computeCuts cut gears =
    Motor.getAllLinks gears
        |> List.filter (Link.cuts cut << Link.toSegment << Gear.toDrawLink gears)


manageInteractEvent : Interact.Event Interactable -> Model -> Mobeel -> Return
manageInteractEvent event model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = Nothing
            , outMsg = Nothing
            , cmd = Cmd.none
            }
    in
    -- TODO Find good pattern for big mess there
    case model.mode of
        CommonMode Nav ->
            { return | outMsg = interactNav event <| Content.M mobile }

        Move ->
            -- FIXME copy of edit move
            case ( event.item, event.action, model.dragging ) of
                -- MOVE
                ( IWheel (G id), Interact.Dragged oldPos newPos _, _ ) ->
                    let
                        gearUp =
                            Gear.update <| Gear.Move <| Vec.sub newPos oldPos
                    in
                    { return
                        | model = { model | dragging = Moving }
                        , mobile = { mobile | gears = Coll.update id gearUp mobile.gears }
                        , toUndo = Group
                    }

                ( _, Interact.DragEnded _, Moving ) ->
                    { return | model = { model | dragging = NoDrag }, toUndo = Do }

                _ ->
                    return

        SelectMotor ->
            case ( event.item, event.action ) of
                ( IWheel (G id), Interact.Clicked _ ) ->
                    { return
                        | model = { model | mode = CommonMode Normal }
                        , mobile = { mobile | motor = id }
                        , toUndo = Do
                    }

                _ ->
                    return

        ChangeSound _ ->
            return

        CommonMode Normal ->
            case model.tool of
                -- PLAY --------
                Play on ->
                    interactPlay on event model mobile

                -- LINK --------
                Harmonize ->
                    interactHarmonize event model mobile

                -- EDIT --------
                Edit ->
                    case ( event.item, event.action, model.dragging ) of
                        -- MOVE
                        ( IWheel (G id), Interact.Dragged oldPos newPos _, _ ) ->
                            let
                                gearUp =
                                    Gear.update <| Gear.Move <| Vec.sub newPos oldPos
                            in
                            { return
                                | model = { model | dragging = Moving }
                                , mobile = { mobile | gears = Coll.update id gearUp mobile.gears }
                                , toUndo = Group
                            }

                        ( _, Interact.DragEnded _, Moving ) ->
                            { return | model = { model | dragging = NoDrag }, toUndo = Do }

                        _ ->
                            { return | model = { model | common = interactSelectEdit event model.common } }


interactPlay : Bool -> Interact.Event Interactable -> Model -> Mobeel -> Return
interactPlay on event model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = Nothing
            , outMsg = Nothing
            , cmd = Cmd.none
            }

        scale =
            PanSvg.getScale model.svg
    in
    case ( event.item, event.action, model.dragging ) of
        -- MUTE
        ( IWheel (G id), Interact.Clicked _, _ ) ->
            let
                w =
                    (Coll.get id mobile.gears).wheel

                newMute =
                    not w.mute
            in
            { return
                | mobile =
                    { mobile
                        | gears =
                            Coll.update id
                                (\g -> { g | wheel = { w | mute = newMute } })
                                mobile.gears
                    }
                , toUndo = Do
                , toEngine = Engine.muted id newMute model.engine
            }

        -- CUT
        ( ISurface, Interact.Dragged p1 p2 _, NoDrag ) ->
            { return | model = { model | dragging = Cut ( p1, p2 ) <| computeCuts ( p1, p2 ) mobile.gears } }

        ( ISurface, Interact.Dragged _ p2 _, Cut ( p1, _ ) _ ) ->
            { return | model = { model | dragging = Cut ( p1, p2 ) <| computeCuts ( p1, p2 ) mobile.gears } }

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
        ( IWheel (G id), Interact.Dragged oldPos newPos ( True, _, _ ), NoDrag ) ->
            let
                res =
                    doVolumeChange id oldPos newPos scale mobile model.engine
            in
            { return
                | model = { model | dragging = VolumeChange }
                , mobile = res.mobile
                , toUndo = res.toUndo
                , toEngine = res.toEngine
            }

        ( IWheel (G id), Interact.Dragged oldPos newPos _, VolumeChange ) ->
            let
                res =
                    doVolumeChange id oldPos newPos scale mobile model.engine
            in
            { return | mobile = res.mobile, toUndo = res.toUndo, toEngine = res.toEngine }

        ( _, Interact.DragEnded _, VolumeChange ) ->
            { return | model = { model | dragging = NoDrag }, toUndo = Do }

        -- LINK -> MOTOR
        ( IWheel _, Interact.Dragged _ _ _, CompleteLink _ ) ->
            -- If ConpleteLink, don’t move
            return

        ( IWheel (G id), Interact.Dragged _ pos _, _ ) ->
            { return | model = { model | dragging = HalfLink ( id, pos ) } }

        ( IWheel (G to), Interact.DragIn, HalfLink ( from, _ ) ) ->
            { return | model = { model | dragging = CompleteLink ( from, to ) } }

        ( IWheel (G to), Interact.DragOut, CompleteLink ( from, _ ) ) ->
            let
                toCenter =
                    (Coll.get to mobile.gears).pos
            in
            { return | model = { model | dragging = HalfLink ( from, toCenter ) } }

        ( IWheel _, Interact.DragEnded True, CompleteLink l ) ->
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


interactHarmonize : Interact.Event Interactable -> Model -> Mobeel -> Return
interactHarmonize event model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = Nothing
            , outMsg = Nothing
            , cmd = Cmd.none
            }
    in
    case ( event.item, event.action, model.dragging ) of
        -- COPY
        ( IWheel (G id), Interact.Clicked _, _ ) ->
            { return | mobile = { mobile | gears = Gear.copy id mobile.gears }, toUndo = Do }

        -- RESIZE
        ( IResizeHandle (G id) add, Interact.Dragged oldPos newPos _, NoDrag ) ->
            { return
                | model = { model | dragging = SizeChange }
                , mobile = doResize id oldPos newPos add mobile
                , toUndo = Group
            }

        ( IResizeHandle (G id) add, Interact.Dragged oldPos newPos _, SizeChange ) ->
            { return | mobile = doResize id oldPos newPos add mobile, toUndo = Group }

        ( _, Interact.DragEnded _, SizeChange ) ->
            { return | model = { model | dragging = NoDrag }, toUndo = Do }

        -- LINK -> HARMO
        ( IWheel _, Interact.Dragged _ _ _, CompleteLink _ ) ->
            -- If Complete Link, don’t move
            return

        ( IWheel (G id), Interact.Dragged _ pos _, _ ) ->
            { return | model = { model | dragging = HalfLink ( id, pos ) } }

        ( IWheel (G to), Interact.DragIn, HalfLink ( from, _ ) ) ->
            { return | model = { model | dragging = CompleteLink ( from, to ) } }

        ( IWheel (G to), Interact.DragOut, CompleteLink ( from, _ ) ) ->
            let
                toCenter =
                    (Coll.get to mobile.gears).pos
            in
            { return | model = { model | dragging = HalfLink ( from, toCenter ) } }

        ( IWheel _, Interact.DragEnded True, CompleteLink l ) ->
            let
                ( newGears, mayFract ) =
                    doLinked l mobile.gears
            in
            { return
                | model =
                    { model
                        | dragging = NoDrag
                        , link = Just { link = l, fract = mayFract }
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
