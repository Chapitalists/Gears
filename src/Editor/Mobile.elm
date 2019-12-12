module Editor.Mobile exposing (..)

import Coll exposing (Coll, Id)
import Collar
import Content
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Engine exposing (Engine)
import Fraction as Fract exposing (Fraction)
import Gear
import Harmony as Harmo
import Interact
import Json.Encode as E
import Link exposing (Link)
import Math.Vector2 as Vec exposing (Vec2)
import Mobile exposing (Geer, Mobeel)
import Motor
import Sound
import TypedSvg.Core exposing (Svg)
import Wheel exposing (Wheel)


type alias Model =
    { dragging : Dragging
    , tool : Tool
    , edit : EditInfo
    , link : Maybe LinkInfo
    , engine : Engine
    , resizing : Maybe (Id Wheel)
    }



-- TODO Mix Dragging inside Tool to make impossible states impossible ?


type Tool
    = Edit
    | Play Bool
    | Harmonize


type EditInfo
    = NoSelection
    | Gear (Id Geer)
    | ChangeSound (Id Geer)


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


type Interactable
    = INothing
    | ISurface
    | IGear (Id Geer)
    | IResizeHandle (Id Geer) Bool


fromGearInteractable : Wheel.Interactable x -> Interactable
fromGearInteractable i =
    case i of
        Wheel.IWheel id ->
            IGear <| Coll.idMap id

        Wheel.IResizeHandle id bool ->
            IResizeHandle (Coll.idMap id) bool


init =
    { dragging = NoDrag
    , tool = Play False
    , edit = NoSelection
    , link = Nothing
    , engine = Engine.init
    , resizing = Nothing
    }


type Msg
    = ChangedTool Tool
      -- TODO EngineMsg ?
    | ToggleEngine
    | PlayGear (Id Geer)
    | StopGear (Id Geer)
      --
    | CopyGear (Id Geer)
    | ChangeSoundStarted (Id Geer)
    | ChangeSoundCanceled (Id Geer)
    | DeleteGear (Id Geer)
    | EnteredFract Bool String -- True for Numerator
    | AppliedFract (Link Geer) Fraction
    | SimplifyFractView
    | Capsuled (Id Geer)
    | Collared (Id Geer)
    | Interacted (Interact.Event Interactable)
    | WheelMsg ( Id Geer, Wheel.Msg )
    | GearMsg ( Id Geer, Gear.Msg )
    | OutMsg DocMsg


type DocMsg
    = Inside (Id Geer)


type ToUndo
    = Do
    | Group
    | NOOP


type alias Return =
    { model : Model
    , mobile : Mobeel
    , toUndo : ToUndo
    , toEngine : Maybe E.Value
    , outMsg : Maybe DocMsg
    }


update : Msg -> Float -> ( Model, Mobeel ) -> Return
update msg scale ( model, mobile ) =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = Nothing
            , outMsg = Nothing
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

        CopyGear id ->
            { return | mobile = { mobile | gears = Gear.copy id mobile.gears }, toUndo = Do }

        ChangeSoundStarted id ->
            { return | model = { model | edit = ChangeSound id } }

        ChangeSoundCanceled id ->
            { return | model = { model | edit = Gear id } }

        DeleteGear id ->
            let
                edit =
                    if model.edit == Gear id then
                        NoSelection

                    else
                        model.edit

                harmo =
                    (Coll.get id mobile.gears).harmony
            in
            if Harmo.hasHarmonics harmo then
                -- TODO
                Debug.log "TODO delete base" return

            else
                case Harmo.getBaseId harmo of
                    Nothing ->
                        { return
                            | model = { model | edit = edit, engine = Engine.init }
                            , mobile = { mobile | gears = Coll.remove id mobile.gears }
                            , toUndo = Do
                            , toEngine = Just Engine.stop
                        }

                    Just baseId ->
                        { return
                            | model = { model | engine = Engine.init }
                            , mobile =
                                { mobile
                                    | gears =
                                        mobile.gears
                                            |> Coll.update baseId (Harmo.remove id)
                                            |> Coll.remove id
                                }
                            , toUndo = Do
                            , toEngine = Just Engine.stop
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

        -- TODO Find good pattern for big mess there
        Interacted event ->
            case model.tool of
                -- PLAY --------
                Play on ->
                    interactPlay on scale event model mobile

                -- LINK --------
                Harmonize ->
                    interactHarmonize scale event model mobile

                -- EDIT --------
                Edit ->
                    case ( event.item, event.action, model.dragging ) of
                        -- DETAIL
                        ( IGear id, Interact.Clicked _, _ ) ->
                            { return | model = { model | edit = Gear id } }

                        -- MOVE
                        ( IGear id, Interact.Dragged oldPos newPos _, _ ) ->
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


viewContent : ( Model, Mobeel ) -> Interact.Interact Interactable -> Float -> List (Svg (Interact.Msg (Wheel.Interactable Geer)))
viewContent ( model, mobile ) inter scale =
    let
        getMod : Interact.Interact Interactable -> Id Geer -> Wheel.Mod
        getMod i id =
            if model.tool == Edit && model.edit == Gear id then
                Wheel.Selected

            else
                case i of
                    Just ( IGear iid, mode ) ->
                        if iid /= id then
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
                        if iid /= id then
                            Wheel.None

                        else
                            Wheel.Resizing

                    _ ->
                        Wheel.None
    in
    (List.map
        (\( id, g ) ->
            Wheel.view g.wheel
                g.pos
                (Harmo.getLength g.harmony mobile.gears)
                { mod = getMod inter id, motor = id == mobile.motor, dashed = Harmo.hasHarmonics g.harmony }
                id
                (Gear.toUID id)
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
                    [ Link.drawCut seg scale ]

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
    case model.tool of
        Edit ->
            case model.edit of
                Gear id ->
                    let
                        g =
                            Coll.get id mobile.gears
                    in
                    [ column [ height fill, Bg.color (rgb 0.5 0.5 0.5), Font.color (rgb 1 1 1), Font.size 16, spacing 20, padding 10 ]
                        [ Input.text [ Font.color (rgb 0 0 0) ]
                            { label = Input.labelAbove [] <| text "Roue :"
                            , text = g.wheel.name
                            , placeholder = Just <| Input.placeholder [] <| text <| Gear.toUID id
                            , onChange = \str -> WheelMsg ( id, Wheel.Named str )
                            }
                        , case Wheel.getContent g of
                            Content.S s ->
                                text <| Sound.toString s

                            Content.M m ->
                                Input.button []
                                    { label = text "Voir Mobile"
                                    , onPress = Just <| OutMsg <| Inside id
                                    }

                            Content.C c ->
                                Input.button []
                                    { label = text "Voir Collier"
                                    , onPress = Just <| OutMsg <| Inside id
                                    }
                        , Input.button []
                            { label = text "PlayPause"
                            , onPress = Just <| PlayGear id
                            }
                        , Input.button []
                            { label = text "Stop"
                            , onPress = Just <| StopGear id
                            }
                        , Input.slider []
                            { label = Input.labelAbove [] <| text "Volume"
                            , onChange = \f -> WheelMsg ( id, Wheel.ChangeVolume f )
                            , value = g.wheel.volume
                            , min = 0
                            , max = 1
                            , step = Just 0.01
                            , thumb = Input.defaultThumb
                            }
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
                        , Input.button []
                            { label = text "Changer son"
                            , onPress = Just <| ChangeSoundStarted id
                            }
                        , Input.button []
                            { label = text "Encapsuler"
                            , onPress = Just <| Capsuled id
                            }
                        , Input.button []
                            { label = text "Collier"
                            , onPress = Just <| Collared id
                            }
                        , Input.button []
                            { onPress = Just <| DeleteGear id
                            , label = text "Supprimer"
                            }
                        ]
                    ]

                ChangeSound id ->
                    [ column [ height fill, Bg.color (rgb 0.5 0.2 0), Font.color (rgb 1 1 1), spacing 20, padding 10 ]
                        [ text <| Gear.toUID id
                        , text "Choisir un son chargé"
                        , Input.button []
                            { label = text "Annuler"
                            , onPress = Just <| ChangeSoundCanceled id
                            }
                        ]
                    ]

                _ ->
                    []

        Harmonize ->
            case model.link of
                Just { link, fract } ->
                    [ column [ height fill, Bg.color (rgb 0.5 0.5 0.5), Font.color (rgb 1 1 1), spacing 20, padding 10 ]
                        ([ text <| (Gear.toUID <| Tuple.first link) ++ (Gear.toUID <| Tuple.second link) ]
                            ++ (case fract of
                                    Nothing ->
                                        [ text "Unrelated" ]

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

        _ ->
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


interactPlay : Bool -> Float -> Interact.Event Interactable -> Model -> Mobeel -> Return
interactPlay on scale event model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = Nothing
            , outMsg = Nothing
            }
    in
    case ( event.item, event.action, model.dragging ) of
        -- MUTE
        ( IGear id, Interact.Clicked _, _ ) ->
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
        ( IGear id, Interact.Dragged oldPos newPos ( True, _, _ ), NoDrag ) ->
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

        ( IGear id, Interact.Dragged oldPos newPos _, VolumeChange ) ->
            let
                res =
                    doVolumeChange id oldPos newPos scale mobile model.engine
            in
            { return | mobile = res.mobile, toUndo = res.toUndo, toEngine = res.toEngine }

        ( _, Interact.DragEnded _, VolumeChange ) ->
            { return | model = { model | dragging = NoDrag }, toUndo = Do }

        -- LINK -> MOTOR
        ( IGear _, Interact.Dragged _ _ _, CompleteLink _ ) ->
            -- If ConpleteLink, don’t move
            return

        ( IGear id, Interact.Dragged _ pos _, _ ) ->
            { return | model = { model | dragging = HalfLink ( id, pos ) } }

        ( IGear to, Interact.DragIn, HalfLink ( from, _ ) ) ->
            { return | model = { model | dragging = CompleteLink ( from, to ) } }

        ( IGear to, Interact.DragOut, CompleteLink ( from, _ ) ) ->
            let
                toCenter =
                    (Coll.get to mobile.gears).pos
            in
            { return | model = { model | dragging = HalfLink ( from, toCenter ) } }

        ( IGear _, Interact.DragEnded True, CompleteLink l ) ->
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


interactHarmonize : Float -> Interact.Event Interactable -> Model -> Mobeel -> Return
interactHarmonize scale event model mobile =
    let
        return =
            { model = model
            , mobile = mobile
            , toUndo = NOOP
            , toEngine = Nothing
            , outMsg = Nothing
            }
    in
    case ( event.item, event.action, model.dragging ) of
        -- COPY
        ( IGear id, Interact.Clicked _, _ ) ->
            { return | mobile = { mobile | gears = Gear.copy id mobile.gears }, toUndo = Do }

        -- RESIZE
        ( IResizeHandle id add, Interact.Dragged oldPos newPos _, NoDrag ) ->
            { return
                | model = { model | dragging = SizeChange }
                , mobile = doResize id oldPos newPos add mobile
                , toUndo = Group
            }

        ( IResizeHandle id add, Interact.Dragged oldPos newPos _, SizeChange ) ->
            { return | mobile = doResize id oldPos newPos add mobile, toUndo = Group }

        ( _, Interact.DragEnded _, SizeChange ) ->
            { return | model = { model | dragging = NoDrag }, toUndo = Do }

        -- LINK -> HARMO
        ( IGear _, Interact.Dragged _ _ _, CompleteLink _ ) ->
            -- If Complete Link, don’t move
            return

        ( IGear id, Interact.Dragged _ pos _, _ ) ->
            { return | model = { model | dragging = HalfLink ( id, pos ) } }

        ( IGear to, Interact.DragIn, HalfLink ( from, _ ) ) ->
            { return | model = { model | dragging = CompleteLink ( from, to ) } }

        ( IGear to, Interact.DragOut, CompleteLink ( from, _ ) ) ->
            let
                toCenter =
                    (Coll.get to mobile.gears).pos
            in
            { return | model = { model | dragging = HalfLink ( from, toCenter ) } }

        ( IGear _, Interact.DragEnded True, CompleteLink l ) ->
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
