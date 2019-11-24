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
        , futureLink : Maybe FutureLink
        , cutting : ( Maybe ( Vec2, Vec2 ), List Link )
        , tool : Tool
        , engine : Engine
        , details : Detailed
        }


type alias Mobile =
    { gears : Coll Gear, motor : Id Gear }


type FutureLink
    = Demi ( Id Gear, Vec2 )
    | Complete Link


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
        , futureLink = Nothing
        , cutting = ( Nothing, [] )
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


update : Msg -> Doc -> ( Doc, Cmd msg )
update msg (D doc) =
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
                    , futureLink =
                        if tool == Edit then
                            Nothing

                        else
                            doc.futureLink
                    , engine = newEngine
                    , cutting = ( Nothing, [] )
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
                (D
                    { doc
                        | data = undoNew doc.data <| \d -> { d | gears = Coll.update id (Gear.update subMsg) d.gears }
                    }
                )

        -- TODO Find good pattern for big mess there
        InteractEvent event ->
            case ( doc.tool, event ) of
                ( Play, Interact.Clicked i ) ->
                    case i of
                        IGear id ->
                            let
                                ( newGears, cmd ) =
                                    Engine.mute id mobile.gears doc.engine
                            in
                            ( D { doc | data = undoNew doc.data (\m -> { m | gears = newGears }) }, cmd )

                        _ ->
                            ( D doc, Cmd.none )

                ( Link, Interact.Clicked i ) ->
                    case i of
                        IGear id ->
                            ( D { doc | data = undoNew doc.data (\m -> { m | gears = Gear.copy id m.gears }) }
                            , Cmd.none
                            )

                        _ ->
                            ( D doc, Cmd.none )

                ( Edit, Interact.Clicked i ) ->
                    case i of
                        IGear id ->
                            ( D { doc | details = DGear id }, Cmd.none )

                        _ ->
                            ( D doc, Cmd.none )

                ( Edit, Interact.Dragged i oldPos newPos ) ->
                    case i of
                        IGear id ->
                            update (GearMsg <| ( id, Gear.Move <| Vec.sub newPos oldPos )) <| D doc

                        _ ->
                            ( D doc, Cmd.none )

                ( _, Interact.Dragged i oldPos newPos ) ->
                    case i of
                        IGear id ->
                            case doc.futureLink of
                                Just (Complete _) ->
                                    ( D doc, Cmd.none )

                                _ ->
                                    ( D { doc | futureLink = Just <| Demi ( id, newPos ) }, Cmd.none )

                        ISurface ->
                            if doc.tool == Play then
                                let
                                    cut =
                                        case Tuple.first doc.cutting of
                                            Nothing ->
                                                ( oldPos, newPos )

                                            Just ( start, _ ) ->
                                                ( start, newPos )

                                    newCuts =
                                        Engine.getAllLinks mobile.gears
                                            |> List.filter (Link.cuts cut << Link.toSegment mobile.gears)
                                in
                                ( D
                                    { doc
                                        | cutting = ( Just cut, newCuts )
                                    }
                                , Cmd.none
                                )

                            else
                                ( D doc, Cmd.none )

                        IResizeHandle id add ->
                            if doc.tool == Link then
                                let
                                    g =
                                        Coll.get id mobile.gears

                                    d =
                                        Vec.getX newPos - Vec.getX oldPos

                                    dd =
                                        if add then
                                            d

                                        else
                                            -d

                                    l =
                                        abs <| dd * 2 + Gear.getLength g mobile.gears
                                in
                                ( D
                                    { doc
                                        | data =
                                            undoNew doc.data
                                                (\m -> { m | gears = Gear.resizeFree id l m.gears })
                                    }
                                , Cmd.none
                                )

                            else
                                ( D doc, Cmd.none )

                        _ ->
                            ( D doc, Cmd.none )

                ( _, Interact.DragIn i ) ->
                    case doc.futureLink of
                        Just (Demi ( idFrom, _ )) ->
                            case i of
                                IGear idTo ->
                                    ( D { doc | futureLink = Just <| Complete ( idFrom, idTo ) }, Cmd.none )

                                _ ->
                                    ( D doc, Cmd.none )

                        _ ->
                            ( D doc, Cmd.none )

                ( _, Interact.DragOut ) ->
                    case doc.futureLink of
                        Just (Complete ( idFrom, idTo )) ->
                            ( D { doc | futureLink = Just <| Demi ( idFrom, Gear.getPos <| Coll.get idTo mobile.gears ) }, Cmd.none )

                        _ ->
                            ( D doc, Cmd.none )

                ( Play, Interact.DragEnded valid ) ->
                    case ( doc.futureLink, valid ) of
                        ( Just (Complete l), True ) ->
                            let
                                ( gears, newEngine, cmd ) =
                                    Engine.addMotor l mobile.gears doc.engine
                            in
                            ( D
                                { doc
                                    | futureLink = Nothing
                                    , data = undoNew doc.data (\m -> { m | gears = gears })
                                    , engine = newEngine
                                }
                            , cmd
                            )

                        _ ->
                            let
                                ( gears, engine, cmd ) =
                                    Engine.rmMotors (Tuple.second doc.cutting) mobile doc.engine
                            in
                            ( D
                                { doc
                                    | futureLink = Nothing
                                    , data = undoNew doc.data (\m -> { m | gears = gears })
                                    , engine = engine
                                    , cutting = ( Nothing, [] )
                                }
                            , cmd
                            )

                ( Link, Interact.DragEnded valid ) ->
                    case ( doc.futureLink, valid ) of
                        ( Just (Complete l), True ) ->
                            let
                                base id =
                                    Maybe.withDefault id <| Gear.getBaseId <| Coll.get id mobile.gears

                                bases =
                                    Tuple.mapBoth base base l

                                upData =
                                    if
                                        (Tuple.first bases == Tuple.second bases)
                                            && (not <| Gear.isActiveLink l <| Coll.get (Tuple.first bases) mobile.gears)
                                    then
                                        undoNew doc.data
                                            (\m ->
                                                { m
                                                    | gears =
                                                        Coll.update
                                                            (Tuple.first bases)
                                                            (Gear.addLink l)
                                                            mobile.gears
                                                }
                                            )

                                    else
                                        doc.data

                                mayFract =
                                    if Tuple.first bases == Tuple.second bases then
                                        Just <|
                                            Fract.division
                                                (Gear.getFract <| Coll.get (Tuple.second l) mobile.gears)
                                                (Gear.getFract <| Coll.get (Tuple.first l) mobile.gears)

                                    else
                                        Nothing
                            in
                            ( D
                                { doc
                                    | futureLink = Nothing
                                    , details = DHarmolink l <| Debug.log "fract" mayFract
                                    , data = upData
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( D { doc | futureLink = Nothing }, Cmd.none )

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
        ++ (case doc.futureLink of
                Nothing ->
                    []

                Just (Demi ( id, pos )) ->
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

                Just (Complete l) ->
                    case doc.tool of
                        Play ->
                            Link.viewMotorLink mobile.gears [] l

                        Link ->
                            Link.viewFractLink mobile.gears l

                        _ ->
                            []
           )
        ++ (case doc.tool of
                Play ->
                    List.concatMap (Link.viewMotorLink mobile.gears (Tuple.second doc.cutting)) <|
                        Engine.getAllLinks mobile.gears

                Link ->
                    List.concatMap (Link.viewFractLink mobile.gears) <|
                        List.concatMap Gear.getGearLinks <|
                            Coll.values mobile.gears

                _ ->
                    []
           )
        ++ (case doc.cutting of
                ( Just seg, _ ) ->
                    [ Link.drawCut seg scale ]

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


undoNew : UndoList model -> (model -> model) -> UndoList model
undoNew undo action =
    Undo.new (action undo.present) undo


undont : UndoList model -> (model -> model) -> UndoList model
undont undo action =
    { undo | present = action undo.present }
