module Doc exposing (..)

import Coll exposing (Coll, Id)
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Engine exposing (Engine)
import Fraction as Fract exposing (Fraction)
import Gear exposing (Gear, Ref)
import Interact
import Link exposing (Link)
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Sound exposing (Sound)
import TypedSvg.Core exposing (Svg)
import UndoList as Undo exposing (UndoList)



-- TODO futureLink and cutting could be a single Variant as it’s impossible to drag 2 things at the same time


type Doc
    = D
        { data : UndoList Mobile
        , dragging : Dragging
        , tool : Tool
        , engine : Engine
        , details : Detailed
        }


type alias Mobile =
    { gears : Coll Gear, motor : Id Gear }


type Dragging
    = NoDrag
    | HalfLink ( Id Gear, Vec2 )
    | CompleteLink Link
    | Cut ( Vec2, Vec2 ) (List Link)


type Tool
    = Edit
    | Play
    | Link


type Interactable
    = INothing
    | ISurface
    | IGear (Id Gear)
    | IResizeHandle (Id Gear) Bool


fromGearInteractable : Gear.Interactable -> Interactable
fromGearInteractable i =
    case i of
        Gear.Gear id ->
            IGear id

        Gear.ResizeHandle id bool ->
            IResizeHandle id bool


type Detailed
    = DNothing
    | DGear (Id Gear)
    | DHarmolink Link (Maybe Fraction)


new : Doc
new =
    D
        { data = Undo.fresh { gears = Coll.empty Gear.default, motor = Coll.startId }
        , dragging = NoDrag
        , tool = Play
        , engine = Engine.init
        , details = DNothing
        }


addNewGear : Sound -> Doc -> ( Doc, Vec2 )
addNewGear sound (D doc) =
    let
        pos =
            vec2 50 50
    in
    ( D
        { doc
            | data =
                undoNew doc.data <|
                    \m -> { m | gears = Coll.insert (Gear.fromSound sound pos) m.gears }
        }
    , pos
    )


type Msg
    = ChangedTool Tool
    | ToggleEngine
    | PlayGear (Id Gear)
    | StopGear (Id Gear)
    | CopyGear (Id Gear)
    | DeleteGear (Id Gear)
    | EnteredFract Bool String -- True for Numerator
    | AppliedFract Link Fraction
    | Undo
    | Redo
    | GearMsg ( Id Gear, Gear.Msg )
    | InteractEvent (Interact.Event Interactable)


update : Msg -> Float -> Doc -> ( Doc, Cmd msg )
update msg scale (D doc) =
    let
        mobile =
            doc.data.present
    in
    case msg of
        ChangedTool tool ->
            let
                ( newEngine, cmd ) =
                    if Engine.isPlaying doc.engine then
                        Engine.toggle mobile doc.engine

                    else
                        ( doc.engine, Cmd.none )
            in
            ( D
                { doc
                    | tool = tool
                    , dragging =
                        case doc.dragging of
                            Cut _ _ ->
                                NoDrag

                            _ ->
                                if tool == Edit then
                                    NoDrag

                                else
                                    doc.dragging
                    , engine = newEngine
                }
            , cmd
            )

        ToggleEngine ->
            let
                ( newEngine, cmd ) =
                    Engine.toggle mobile doc.engine
            in
            ( D { doc | engine = newEngine }, cmd )

        PlayGear id ->
            ( D doc
            , Engine.playPause [ id ] mobile.gears
            )

        StopGear id ->
            ( D doc
            , Engine.stop
            )

        CopyGear id ->
            ( D { doc | data = undoNew doc.data (\m -> { m | gears = Gear.copy id m.gears }) }, Cmd.none )

        DeleteGear id ->
            let
                details =
                    if doc.details == DGear id then
                        DNothing

                    else
                        doc.details

                g =
                    Coll.get id mobile.gears
            in
            if Gear.hasHarmonics g then
                -- TODO
                Debug.log "TODO delete base" ( D doc, Cmd.none )

            else
                case Gear.getBaseId g of
                    Nothing ->
                        ( D
                            { doc
                                | data = undoNew doc.data (\d -> { d | gears = Coll.remove id d.gears })
                                , details = details
                            }
                        , Cmd.none
                        )

                    Just baseId ->
                        ( D
                            { doc
                                | data =
                                    undoNew doc.data <|
                                        \d ->
                                            { d
                                                | gears =
                                                    d.gears
                                                        |> Coll.update baseId (Gear.removeFromRefGroup id)
                                                        |> Coll.remove id
                                            }
                                , details = details
                            }
                        , Cmd.none
                        )

        EnteredFract isNumerator str ->
            case ( doc.details, String.toInt str ) of
                ( DHarmolink l (Just fract), Just i ) ->
                    ( D
                        { doc
                            | details =
                                DHarmolink l <|
                                    Just <|
                                        Fract.fromRecord <|
                                            if isNumerator then
                                                { num = i, den = Fract.getDenominator fract }

                                            else
                                                { num = Fract.getNumerator fract, den = i }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( D doc, Cmd.none )

        AppliedFract l fract ->
            let
                newFract =
                    Fract.multiplication fract <| Gear.getFract <| Coll.get (Tuple.first l) mobile.gears
            in
            ( D
                { doc
                    | data =
                        undoNew doc.data
                            (\m ->
                                { m
                                    | gears =
                                        Coll.update
                                            (Tuple.second l)
                                            (Gear.setFract newFract)
                                            m.gears
                                }
                            )
                }
            , Cmd.none
            )

        Undo ->
            ( D { doc | data = Undo.undo doc.data }, Cmd.none )

        Redo ->
            ( D { doc | data = Undo.redo doc.data }, Cmd.none )

        GearMsg ( id, subMsg ) ->
            update (StopGear id)
                scale
                (D
                    { doc
                        | data = undoNew doc.data <| \d -> { d | gears = Coll.update id (Gear.update subMsg) d.gears }
                    }
                )

        -- TODO Find good pattern for big mess there
        InteractEvent event ->
            case doc.tool of
                -- PLAY --------
                Play ->
                    case ( event.item, event.action, doc.dragging ) of
                        -- MUTE
                        ( IGear id, Interact.Clicked _, _ ) ->
                            let
                                ( newGears, cmd ) =
                                    Engine.mute id mobile.gears doc.engine
                            in
                            ( D { doc | data = undoNew doc.data (\m -> { m | gears = newGears }) }, cmd )

                        -- CUT
                        ( ISurface, Interact.Dragged _ p2 _, Cut ( p1, _ ) _ ) ->
                            ( D { doc | dragging = Cut ( p1, p2 ) <| computeCuts ( p1, p2 ) mobile.gears }, Cmd.none )

                        ( ISurface, Interact.DragEnded True, Cut _ cuts ) ->
                            let
                                ( gears, engine, cmd ) =
                                    Engine.rmMotors cuts mobile doc.engine
                            in
                            ( D
                                { doc
                                    | dragging = NoDrag
                                    , data = undoNew doc.data (\m -> { m | gears = gears })
                                    , engine = engine
                                }
                            , cmd
                            )

                        -- VOLUME
                        ( IGear id, Interact.Dragged oldPos newPos ( True, _, _ ), NoDrag ) ->
                            let
                                volume =
                                    computeVolume (Gear.getVolume <| Coll.get id mobile.gears) oldPos newPos scale

                                gears =
                                    Coll.update id (Gear.update <| Gear.ChangeVolume volume) mobile.gears
                            in
                            ( D { doc | data = undoNew doc.data (\m -> { m | gears = gears }) }
                            , Engine.volumeChanged id volume doc.engine
                            )

                        -- LINK -> MOTOR
                        ( IGear _, Interact.Dragged _ _ _, CompleteLink _ ) ->
                            ( D doc, Cmd.none )

                        ( IGear id, Interact.Dragged _ pos _, _ ) ->
                            ( D { doc | dragging = HalfLink ( id, pos ) }, Cmd.none )

                        ( IGear to, Interact.DragIn, HalfLink ( from, _ ) ) ->
                            ( D { doc | dragging = CompleteLink ( from, to ) }, Cmd.none )

                        ( IGear to, Interact.DragOut, CompleteLink ( from, _ ) ) ->
                            let
                                toCenter =
                                    Gear.getPos (Coll.get to mobile.gears)
                            in
                            ( D { doc | dragging = HalfLink ( from, toCenter ) }, Cmd.none )

                        ( IGear _, Interact.DragEnded True, CompleteLink l ) ->
                            let
                                ( gears, newEngine, cmd ) =
                                    Engine.addMotor l mobile.gears doc.engine
                            in
                            ( D
                                { doc
                                    | dragging = NoDrag
                                    , data = undoNew doc.data (\m -> { m | gears = gears })
                                    , engine = newEngine
                                }
                            , cmd
                            )

                        -- CLEAN DRAG
                        ( _, Interact.DragEnded _, _ ) ->
                            ( D { doc | dragging = NoDrag }, Cmd.none )

                        _ ->
                            ( D doc, Cmd.none )

                -- LINK --------
                Link ->
                    case ( event.item, event.action, doc.dragging ) of
                        -- COPY
                        ( IGear id, Interact.Clicked _, _ ) ->
                            ( D { doc | data = undoNew doc.data (\m -> { m | gears = Gear.copy id m.gears }) }
                            , Cmd.none
                            )

                        -- RESIZE
                        ( IResizeHandle id add, Interact.Dragged oldPos newPos _, _ ) ->
                            let
                                newSize =
                                    computeResize (Gear.getLengthId id mobile.gears) oldPos newPos add
                            in
                            ( D
                                { doc
                                    | data =
                                        undoNew doc.data
                                            (\m -> { m | gears = Gear.resizeFree id newSize m.gears })
                                }
                            , Cmd.none
                            )

                        -- LINK -> HARMO
                        ( IGear _, Interact.Dragged _ _ _, CompleteLink _ ) ->
                            ( D doc, Cmd.none )

                        ( IGear id, Interact.Dragged _ pos _, _ ) ->
                            ( D { doc | dragging = HalfLink ( id, pos ) }, Cmd.none )

                        ( IGear to, Interact.DragIn, HalfLink ( from, _ ) ) ->
                            ( D { doc | dragging = CompleteLink ( from, to ) }, Cmd.none )

                        ( IGear to, Interact.DragOut, CompleteLink ( from, _ ) ) ->
                            let
                                toCenter =
                                    Gear.getPos (Coll.get to mobile.gears)
                            in
                            ( D { doc | dragging = HalfLink ( from, toCenter ) }, Cmd.none )

                        ( IGear _, Interact.DragEnded True, CompleteLink l ) ->
                            let
                                ( newGears, mayFract ) =
                                    doLinked l mobile.gears
                            in
                            ( D
                                { doc
                                    | dragging = NoDrag
                                    , data = undoNew doc.data (\m -> { m | gears = newGears })
                                    , details = DHarmolink l mayFract
                                }
                            , Cmd.none
                            )

                        -- CLEAN DRAG
                        ( _, Interact.DragEnded _, _ ) ->
                            ( D { doc | dragging = NoDrag }, Cmd.none )

                        _ ->
                            ( D doc, Cmd.none )

                -- EDIT --------
                Edit ->
                    case ( event.item, event.action ) of
                        -- DETAIL
                        ( IGear id, Interact.Clicked _ ) ->
                            ( D { doc | details = DGear id }, Cmd.none )

                        -- MOVE
                        ( IGear id, Interact.Dragged oldPos newPos _ ) ->
                            update (GearMsg <| ( id, Gear.Move <| Vec.sub newPos oldPos )) scale <| D doc

                        _ ->
                            ( D doc, Cmd.none )


viewTools : Doc -> Element Msg
viewTools (D doc) =
    row [ width fill, padding 10, spacing 20 ]
        [ Input.radioRow [ spacing 30 ]
            { onChange = ChangedTool
            , options =
                [ Input.option Play <| text "Jeu"
                , Input.option Link <| text "Harmonie"
                , Input.option Edit <| text "Édition"
                ]
            , selected = Just doc.tool
            , label = Input.labelHidden "Outils"
            }
        , Input.button [ alignRight ]
            { label = text "Undo"
            , onPress =
                if Undo.hasPast doc.data then
                    Just Undo

                else
                    Nothing
            }
        , Input.button []
            { label = text "Redo"
            , onPress =
                if Undo.hasFuture doc.data then
                    Just Redo

                else
                    Nothing
            }
        ]


viewExtraTools (D doc) =
    row [ width fill, padding 20 ]
        (if doc.tool == Play then
            [ Input.button [ centerX ]
                { label =
                    if Engine.isPlaying doc.engine then
                        text "Stop"

                    else
                        text "Jouer"
                , onPress = Just ToggleEngine
                }
            ]

         else
            []
        )


viewContent : Doc -> Interact.Interact Interactable -> Float -> List (Svg (Interact.Msg Gear.Interactable))
viewContent (D doc) inter scale =
    let
        mobile =
            doc.data.present

        getMod : Interact.Interact Interactable -> Id Gear -> Gear.Mod
        getMod i id =
            case i of
                Just ( IGear iid, mode ) ->
                    if iid /= id then
                        Gear.None

                    else
                        case ( doc.tool, mode ) of
                            ( Link, Interact.Hover ) ->
                                Gear.Resizable

                            ( _, Interact.Hover ) ->
                                Gear.Hovered

                            ( _, Interact.Click ) ->
                                Gear.Clicked

                            ( _, Interact.Drag ) ->
                                Gear.Dragged

                _ ->
                    Gear.None
    in
    (List.map (\( id, g ) -> Gear.view ( id, g ) mobile.gears (getMod inter id)) <|
        Coll.toList mobile.gears
    )
        ++ (case doc.dragging of
                NoDrag ->
                    []

                HalfLink ( id, pos ) ->
                    let
                        g =
                            Coll.get id mobile.gears
                    in
                    case doc.tool of
                        Play ->
                            let
                                length =
                                    Gear.getLength g mobile.gears
                            in
                            [ Link.drawMotorLink ( ( Gear.getPos g, length ), ( pos, length ) ) ]

                        Link ->
                            [ Link.drawRawLink
                                ( Gear.getPos g, pos )
                                (Gear.getLength g mobile.gears)
                            ]

                        _ ->
                            []

                CompleteLink l ->
                    case doc.tool of
                        Play ->
                            Link.viewMotorLink mobile.gears [] l

                        Link ->
                            Link.viewFractLink mobile.gears l

                        _ ->
                            []

                Cut seg _ ->
                    [ Link.drawCut seg scale ]
           )
        ++ (case doc.tool of
                Play ->
                    let
                        cuts =
                            case doc.dragging of
                                Cut _ c ->
                                    c

                                _ ->
                                    []
                    in
                    List.concatMap (Link.viewMotorLink mobile.gears cuts) <|
                        Engine.getAllLinks mobile.gears

                Link ->
                    List.concatMap (Link.viewFractLink mobile.gears) <|
                        List.concatMap Gear.getGearLinks <|
                            Coll.values mobile.gears

                _ ->
                    []
           )


viewDetails : Doc -> List (Element Msg)
viewDetails (D doc) =
    case doc.details of
        DGear id ->
            [ column [ height fill, Bg.color (rgb 0.5 0.5 0.5), Font.color (rgb 1 1 1), spacing 20, padding 10 ]
                [ text <| Gear.toUID id
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
                    , onChange = \f -> GearMsg ( id, Gear.ChangeVolume f )
                    , value = Gear.getVolume (Coll.get id doc.data.present.gears)
                    , min = 0
                    , max = 1
                    , step = Just 0.01
                    , thumb = Input.defaultThumb
                    }
                , Input.button []
                    { label = text "Copie"
                    , onPress = Just <| CopyGear id
                    }
                , Input.button []
                    { label = text "x 2"
                    , onPress = Just <| GearMsg ( id, Gear.ResizeFract <| Fract.integer 2 )
                    }
                , Input.button []
                    { label = text "/ 2"
                    , onPress = Just <| GearMsg ( id, Gear.ResizeFract <| Fract.unit 2 )
                    }
                , Input.button []
                    { onPress = Just <| DeleteGear id
                    , label = text "Supprimer"
                    }
                ]
            ]

        DHarmolink l mayFract ->
            [ column [ height fill, Bg.color (rgb 0.5 0.5 0.5), Font.color (rgb 1 1 1), spacing 20, padding 10 ]
                ([ text <| (Gear.toUID <| Tuple.first l) ++ (Gear.toUID <| Tuple.second l) ]
                    ++ (case mayFract of
                            Nothing ->
                                [ text "Unrelated" ]

                            Just fract ->
                                [ Input.text [ Font.color (rgb 0 0 0) ]
                                    { text = String.fromInt <| Fract.getNumerator fract
                                    , onChange = EnteredFract True
                                    , label = Input.labelHidden "Numerator"
                                    , placeholder = Nothing
                                    }
                                , text "/"
                                , Input.text [ Font.color (rgb 0 0 0) ]
                                    { text = String.fromInt <| Fract.getDenominator fract
                                    , onChange = EnteredFract False
                                    , label = Input.labelHidden "Denominator"
                                    , placeholder = Nothing
                                    }
                                , Input.button [] { label = text "Change", onPress = Just <| AppliedFract l fract }
                                ]
                       )
                    ++ [ row [] [] ]
                )
            ]

        _ ->
            []


doLinked : Link -> Coll Gear -> ( Coll Gear, Maybe Fraction )
doLinked l gears =
    let
        base id =
            Maybe.withDefault id <| Gear.getBaseId <| Coll.get id gears

        bases =
            Tuple.mapBoth base base l
    in
    ( if
        (Tuple.first bases == Tuple.second bases)
            && (not <| Gear.isActiveLink l <| Coll.get (Tuple.first bases) gears)
      then
        Coll.update (Tuple.first bases) (Gear.addLink l) gears

      else
        gears
    , if Tuple.first bases == Tuple.second bases then
        Just <|
            Fract.division
                (Gear.getFract <| Coll.get (Tuple.second l) gears)
                (Gear.getFract <| Coll.get (Tuple.first l) gears)

      else
        Nothing
    )


computeCuts : ( Vec2, Vec2 ) -> Coll Gear -> List Link
computeCuts cut gears =
    Engine.getAllLinks gears
        |> List.filter (Link.cuts cut << Link.toSegment gears)


computeResize : Float -> Vec2 -> Vec2 -> Bool -> Float
computeResize length oldPos newPos add =
    let
        d =
            Vec.getX newPos - Vec.getX oldPos

        dd =
            if add then
                d

            else
                -d
    in
    abs <| dd * 2 + length


computeVolume volume oldPos newPos scale =
    volume + (Vec.getY oldPos - Vec.getY newPos) / scale / 100


undoNew : UndoList model -> (model -> model) -> UndoList model
undoNew undo action =
    Undo.new (action undo.present) undo


undont : UndoList model -> (model -> model) -> UndoList model
undont undo action =
    { undo | present = action undo.present }
