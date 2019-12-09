module Editor.Mobile exposing (..)

import Coll exposing (Coll, Id)
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


update : Msg -> Float -> ( Model, Mobeel ) -> ( Model, ( Mobeel, ToUndo ), Maybe E.Value )
update msg scale ( model, mobile ) =
    case msg of
        ChangedTool tool ->
            ( { model
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
            , ( mobile, NOOP )
            , Just Engine.stop
            )

        ToggleEngine ->
            case model.tool of
                Play True ->
                    ( { model | tool = Play False, engine = Engine.init }, ( mobile, NOOP ), Just Engine.stop )

                Play False ->
                    let
                        ( engine, v ) =
                            Engine.addPlaying
                                (Motor.getMotored mobile.motor mobile.gears)
                                mobile.gears
                                model.engine
                    in
                    ( { model | tool = Play True, engine = engine }, ( mobile, NOOP ), v )

                _ ->
                    ( model, ( mobile, NOOP ), Nothing )

        PlayGear id ->
            let
                ( engine, v ) =
                    Engine.addPlaying [ id ] mobile.gears model.engine
            in
            ( { model | engine = engine }, ( mobile, NOOP ), v )

        StopGear id ->
            ( { model | engine = Engine.init }, ( mobile, NOOP ), Just Engine.stop )

        CopyGear id ->
            ( model, ( { mobile | gears = Gear.copy id mobile.gears }, Do ), Nothing )

        ChangeSoundStarted id ->
            ( { model | edit = ChangeSound id }, ( mobile, NOOP ), Nothing )

        ChangeSoundCanceled id ->
            ( { model | edit = Gear id }, ( mobile, NOOP ), Nothing )

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
                Debug.log "TODO delete base" ( model, ( mobile, NOOP ), Nothing )

            else
                case Harmo.getBaseId harmo of
                    Nothing ->
                        ( { model | edit = edit, engine = Engine.init }
                        , ( { mobile | gears = Coll.remove id mobile.gears }, Do )
                        , Just Engine.stop
                        )

                    Just baseId ->
                        ( { model | engine = Engine.init }
                        , ( { mobile
                                | gears =
                                    mobile.gears
                                        |> Coll.update baseId (Harmo.remove id)
                                        |> Coll.remove id
                            }
                          , Do
                          )
                        , Just Engine.stop
                        )

        EnteredFract isNumerator str ->
            case ( model.link, String.toInt str ) of
                ( Just link, Just i ) ->
                    case link.fract of
                        Just fract ->
                            ( { model
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
                            , ( mobile, NOOP )
                            , Nothing
                            )

                        _ ->
                            ( model, ( mobile, NOOP ), Nothing )

                _ ->
                    ( model, ( mobile, NOOP ), Nothing )

        AppliedFract l fract ->
            let
                newFract =
                    Fract.multiplication fract <| Harmo.getFract <| Coll.get (Tuple.first l) mobile.gears
            in
            ( model
            , ( { mobile | gears = Coll.update (Tuple.second l) (Harmo.setFract newFract) mobile.gears }, Do )
            , Nothing
            )

        SimplifyFractView ->
            case model.link of
                Just link ->
                    case link.fract of
                        Just fract ->
                            ( { model | link = Just { link | fract = Just <| Fract.simplify fract } }
                            , ( mobile, NOOP )
                            , Nothing
                            )

                        _ ->
                            ( model, ( mobile, NOOP ), Nothing )

                _ ->
                    ( model, ( mobile, NOOP ), Nothing )

        Capsuled id ->
            let
                g =
                    Coll.get id mobile.gears

                newG =
                    { g | motor = Motor.default, harmony = Harmo.newSelf <| Harmo.getLength g.harmony mobile.gears }
            in
            ( model
            , ( { mobile
                    | gears =
                        Coll.update id
                            (Wheel.setContent <| Content.M <| Mobile.fromGear newG)
                            mobile.gears
                }
              , Do
              )
            , Nothing
            )

        WheelMsg ( id, subMsg ) ->
            ( model, ( { mobile | gears = Coll.update id (Wheel.update subMsg) mobile.gears }, Do ), Nothing )

        GearMsg ( id, subMsg ) ->
            ( model, ( { mobile | gears = Coll.update id (Gear.update subMsg) mobile.gears }, Do ), Nothing )

        OutMsg _ ->
            ( model, ( mobile, NOOP ), Nothing )

        -- TODO Find good pattern for big mess there
        Interacted event ->
            case model.tool of
                -- PLAY --------
                Play on ->
                    case ( event.item, event.action, model.dragging ) of
                        -- MUTE
                        ( IGear id, Interact.Clicked _, _ ) ->
                            let
                                w =
                                    (Coll.get id mobile.gears).wheel

                                newMute =
                                    not w.mute
                            in
                            ( model
                            , ( { mobile
                                    | gears =
                                        Coll.update id
                                            (\g -> { g | wheel = { w | mute = newMute } })
                                            mobile.gears
                                }
                              , Do
                              )
                            , Engine.muted id newMute model.engine
                            )

                        -- CUT
                        ( ISurface, Interact.Dragged p1 p2 _, NoDrag ) ->
                            ( { model | dragging = Cut ( p1, p2 ) <| computeCuts ( p1, p2 ) mobile.gears }
                            , ( mobile, NOOP )
                            , Nothing
                            )

                        ( ISurface, Interact.Dragged _ p2 _, Cut ( p1, _ ) _ ) ->
                            ( { model | dragging = Cut ( p1, p2 ) <| computeCuts ( p1, p2 ) mobile.gears }
                            , ( mobile, NOOP )
                            , Nothing
                            )

                        ( ISurface, Interact.DragEnded True, Cut _ cuts ) ->
                            let
                                ( gears, motored ) =
                                    Motor.remove cuts mobile.motor mobile.gears on

                                ( engine, v ) =
                                    Engine.setPlaying motored mobile.gears model.engine
                            in
                            ( { model | dragging = NoDrag, engine = engine }, ( { mobile | gears = gears }, Do ), v )

                        -- VOLUME
                        ( IGear id, Interact.Dragged oldPos newPos ( True, _, _ ), NoDrag ) ->
                            let
                                res =
                                    doVolumeChange id oldPos newPos scale mobile model.engine
                            in
                            ( { model | dragging = VolumeChange }, Tuple.first res, Tuple.second res )

                        ( IGear id, Interact.Dragged oldPos newPos _, VolumeChange ) ->
                            let
                                res =
                                    doVolumeChange id oldPos newPos scale mobile model.engine
                            in
                            ( model, Tuple.first res, Tuple.second res )

                        ( _, Interact.DragEnded _, VolumeChange ) ->
                            ( { model | dragging = NoDrag }, ( mobile, Do ), Nothing )

                        -- LINK -> MOTOR
                        ( IGear _, Interact.Dragged _ _ _, CompleteLink _ ) ->
                            ( model, ( mobile, NOOP ), Nothing )

                        ( IGear id, Interact.Dragged _ pos _, _ ) ->
                            ( { model | dragging = HalfLink ( id, pos ) }, ( mobile, NOOP ), Nothing )

                        ( IGear to, Interact.DragIn, HalfLink ( from, _ ) ) ->
                            ( { model | dragging = CompleteLink ( from, to ) }, ( mobile, NOOP ), Nothing )

                        ( IGear to, Interact.DragOut, CompleteLink ( from, _ ) ) ->
                            let
                                toCenter =
                                    (Coll.get to mobile.gears).pos
                            in
                            ( { model | dragging = HalfLink ( from, toCenter ) }, ( mobile, NOOP ), Nothing )

                        ( IGear _, Interact.DragEnded True, CompleteLink l ) ->
                            let
                                ( gears, toPlay ) =
                                    Motor.add l mobile.gears <| Engine.playingIds model.engine

                                ( engine, v ) =
                                    Engine.addPlaying toPlay mobile.gears model.engine
                            in
                            ( { model | dragging = NoDrag, engine = engine }
                            , ( { mobile | gears = gears }, Do )
                            , v
                            )

                        -- CLEAN DRAG
                        ( _, Interact.DragEnded _, _ ) ->
                            ( { model | dragging = NoDrag }, ( mobile, NOOP ), Nothing )

                        _ ->
                            ( model, ( mobile, NOOP ), Nothing )

                -- LINK --------
                Harmonize ->
                    case ( event.item, event.action, model.dragging ) of
                        -- COPY
                        ( IGear id, Interact.Clicked _, _ ) ->
                            ( model, ( { mobile | gears = Gear.copy id mobile.gears }, Do ), Nothing )

                        -- RESIZE
                        ( IResizeHandle id add, Interact.Dragged oldPos newPos _, NoDrag ) ->
                            ( { model | dragging = SizeChange }
                            , ( doResize id oldPos newPos add mobile, Group )
                            , Nothing
                            )

                        ( IResizeHandle id add, Interact.Dragged oldPos newPos _, SizeChange ) ->
                            ( model, ( doResize id oldPos newPos add mobile, Group ), Nothing )

                        ( _, Interact.DragEnded _, SizeChange ) ->
                            ( { model | dragging = NoDrag }, ( mobile, Do ), Nothing )

                        -- LINK -> HARMO
                        ( IGear _, Interact.Dragged _ _ _, CompleteLink _ ) ->
                            ( model, ( mobile, NOOP ), Nothing )

                        ( IGear id, Interact.Dragged _ pos _, _ ) ->
                            ( { model | dragging = HalfLink ( id, pos ) }, ( mobile, NOOP ), Nothing )

                        ( IGear to, Interact.DragIn, HalfLink ( from, _ ) ) ->
                            ( { model | dragging = CompleteLink ( from, to ) }, ( mobile, NOOP ), Nothing )

                        ( IGear to, Interact.DragOut, CompleteLink ( from, _ ) ) ->
                            let
                                toCenter =
                                    (Coll.get to mobile.gears).pos
                            in
                            ( { model | dragging = HalfLink ( from, toCenter ) }, ( mobile, NOOP ), Nothing )

                        ( IGear _, Interact.DragEnded True, CompleteLink l ) ->
                            let
                                ( newGears, mayFract ) =
                                    doLinked l mobile.gears
                            in
                            ( { model
                                | dragging = NoDrag
                                , link = Just { link = l, fract = mayFract }
                              }
                            , ( { mobile | gears = newGears }, Do )
                              -- TODO if doLinked returns gears unchanged, empty undo
                            , Nothing
                            )

                        -- CLEAN DRAG
                        ( _, Interact.DragEnded _, _ ) ->
                            ( { model | dragging = NoDrag }, ( mobile, NOOP ), Nothing )

                        _ ->
                            ( model, ( mobile, NOOP ), Nothing )

                -- EDIT --------
                Edit ->
                    case ( event.item, event.action, model.dragging ) of
                        -- DETAIL
                        ( IGear id, Interact.Clicked _, _ ) ->
                            ( { model | edit = Gear id }, ( mobile, NOOP ), Nothing )

                        -- MOVE
                        ( IGear id, Interact.Dragged oldPos newPos _, _ ) ->
                            let
                                gearUp =
                                    Gear.update <| Gear.Move <| Vec.sub newPos oldPos
                            in
                            ( { model | dragging = Moving }
                            , ( { mobile | gears = Coll.update id gearUp mobile.gears }, Group )
                            , Nothing
                            )

                        ( _, Interact.DragEnded _, Moving ) ->
                            ( { model | dragging = NoDrag }, ( mobile, Do ), Nothing )

                        _ ->
                            ( model, ( mobile, NOOP ), Nothing )


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

                            _ ->
                                Debug.todo "nav to collar"
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


doVolumeChange : Id Geer -> Vec2 -> Vec2 -> Float -> Mobeel -> Engine -> ( ( Mobeel, ToUndo ), Maybe E.Value )
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
    ( ( { mobile | gears = newGears }, Group ), Engine.volumeChanged id volume engine )


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
