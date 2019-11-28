module Doc exposing (..)

import Coll exposing (Coll, Id)
import Data exposing (Data)
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Engine exposing (Engine)
import Fraction as Fract exposing (Fraction)
import Gear exposing (Gear, Ref)
import Interact
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Link exposing (Link)
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Sound exposing (Sound)
import TypedSvg.Core exposing (Svg)
import Url exposing (Url)



-- TODO futureLink and cutting could be a single Variant as it’s impossible to drag 2 things at the same time


type Doc
    = D
        { data : Data Mobile
        , dragging : Dragging
        , tool : Tool
        , engine : Engine
        , details : Detailed
        , resizing : Maybe (Id Gear)
        }


type alias Mobile =
    { gears : Coll Gear, motor : Id Gear }


changeMobile : Mobile -> String -> Maybe Url -> Doc -> Doc
changeMobile m name url (D d) =
    D { d | data = Data.load m name url }


mobileEncoder : Mobile -> E.Value
mobileEncoder m =
    E.object
        [ ( "motor", Coll.idEncoder m.motor )
        , ( "gears", Coll.encoder m.gears Gear.encoderToSave )
        ]


mobileDecoder : D.Decoder Mobile
mobileDecoder =
    D.succeed Mobile
        |> required "gears" (Coll.decoder Gear.decoder Gear.default)
        |> required "motor" Coll.idDecoder


type Dragging
    = NoDrag
    | HalfLink ( Id Gear, Vec2 )
    | CompleteLink Link
    | Cut ( Vec2, Vec2 ) (List Link)
    | VolumeChange
    | SizeChange
    | Moving


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
        Gear.IGear id ->
            IGear id

        Gear.IResizeHandle id bool ->
            IResizeHandle id bool


type Detailed
    = DNothing
    | DGear (Id Gear)
    | DChangeSound (Id Gear)
    | DHarmolink Link (Maybe Fraction)


new : Maybe Url -> Doc
new url =
    D
        { data = Data.init { gears = Coll.empty Gear.default, motor = Coll.startId } url
        , dragging = NoDrag
        , tool = Play
        , engine = Engine.init
        , details = DNothing
        , resizing = Nothing
        }


soundClicked : Sound -> Doc -> ( Doc, Maybe Vec2 )
soundClicked sound (D doc) =
    case doc.details of
        DChangeSound id ->
            let
                harmos =
                    Gear.getHarmonicGroup id (Data.current doc.data).gears

                chSound =
                    Gear.update <| Gear.ChangeSound sound
            in
            ( D
                { doc
                    | data =
                        updateGears doc.data <|
                            \coll -> List.foldl (\el -> Coll.update el chSound) coll harmos
                    , details = DGear id
                }
            , Nothing
            )

        _ ->
            let
                pos =
                    vec2 50 50
            in
            ( D { doc | data = updateGears doc.data <| Coll.insert <| Gear.fromSound sound pos }
            , Just pos
            )


type Msg
    = ChangedTool Tool
    | EnteredFileName String
    | Save
    | Saved
    | ToggleEngine
    | PlayGear (Id Gear)
    | StopGear (Id Gear)
    | CopyGear (Id Gear)
    | ChangeSound (Id Gear)
    | ChangeSoundCanceled (Id Gear)
    | DeleteGear (Id Gear)
    | EnteredFract Bool String -- True for Numerator
    | AppliedFract Link Fraction
    | SimplifyFractView
    | Undo
    | Redo
    | GearMsg ( Id Gear, Gear.Msg )
    | InteractEvent (Interact.Event Interactable)


update : Msg -> Float -> Doc -> ( Doc, Cmd Msg )
update msg scale (D doc) =
    let
        mobile =
            Data.current doc.data
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

        EnteredFileName name ->
            if String.all (\c -> Char.isAlphaNum c || c == '-') name then
                ( D { doc | data = Data.setName name doc.data }, Cmd.none )

            else
                ( D doc, Cmd.none )

        Save ->
            let
                ( data, cmd ) =
                    Data.save doc.data mobileEncoder Saved
            in
            ( D { doc | data = data }, cmd )

        Saved ->
            --TODO handle server response
            ( D doc, Cmd.none )

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
            ( D { doc | data = updateGears doc.data <| Gear.copy id }, Cmd.none )

        ChangeSound id ->
            ( D { doc | details = DChangeSound id }, Cmd.none )

        ChangeSoundCanceled id ->
            ( D { doc | details = DGear id }, Cmd.none )

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
                                | data = updateGears doc.data <| Coll.remove id
                                , details = details
                            }
                        , Cmd.none
                        )

                    Just baseId ->
                        ( D
                            { doc
                                | data =
                                    updateGears doc.data <|
                                        Coll.update baseId (Gear.removeFromRefGroup id)
                                            >> Coll.remove id
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
                                        if isNumerator then
                                            { num = i, den = fract.den }

                                        else
                                            { num = fract.num, den = i }
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
                        updateGears doc.data <|
                            Coll.update
                                (Tuple.second l)
                                (Gear.setFract newFract)
                }
            , Cmd.none
            )

        SimplifyFractView ->
            case doc.details of
                DHarmolink l (Just fract) ->
                    ( D { doc | details = DHarmolink l <| Just <| Fract.simplify fract }, Cmd.none )

                _ ->
                    ( D doc, Cmd.none )

        Undo ->
            ( D { doc | data = Data.undo doc.data }, Cmd.none )

        Redo ->
            ( D { doc | data = Data.redo doc.data }, Cmd.none )

        GearMsg ( id, subMsg ) ->
            update (StopGear id)
                scale
                (D { doc | data = updateGears doc.data <| Coll.update id (Gear.update subMsg) })

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
                            ( D { doc | data = updateGears doc.data <| always newGears }, cmd )

                        -- CUT
                        ( ISurface, Interact.Dragged p1 p2 _, NoDrag ) ->
                            ( D { doc | dragging = Cut ( p1, p2 ) <| computeCuts ( p1, p2 ) mobile.gears }, Cmd.none )

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
                                    , data = updateGears doc.data <| always gears
                                    , engine = engine
                                }
                            , cmd
                            )

                        -- VOLUME
                        ( IGear id, Interact.Dragged oldPos newPos ( True, _, _ ), NoDrag ) ->
                            doVolumeChange id oldPos newPos scale (D { doc | dragging = VolumeChange })

                        ( IGear id, Interact.Dragged oldPos newPos _, VolumeChange ) ->
                            doVolumeChange id oldPos newPos scale (D doc)

                        ( _, Interact.DragEnded _, VolumeChange ) ->
                            ( D { doc | dragging = NoDrag, data = Data.do mobile doc.data }, Cmd.none )

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
                                    , data = updateGears doc.data <| always gears
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
                            ( D { doc | data = updateGears doc.data <| Gear.copy id }
                            , Cmd.none
                            )

                        -- RESIZE
                        ( IResizeHandle id add, Interact.Dragged oldPos newPos _, NoDrag ) ->
                            ( doResize id oldPos newPos add (D { doc | dragging = SizeChange }), Cmd.none )

                        ( IResizeHandle id add, Interact.Dragged oldPos newPos _, SizeChange ) ->
                            ( doResize id oldPos newPos add (D doc), Cmd.none )

                        ( _, Interact.DragEnded _, SizeChange ) ->
                            ( D { doc | dragging = NoDrag, data = Data.do mobile doc.data }, Cmd.none )

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
                                    , data = updateGears doc.data <| always newGears
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
                    case ( event.item, event.action, doc.dragging ) of
                        -- DETAIL
                        ( IGear id, Interact.Clicked _, _ ) ->
                            ( D { doc | details = DGear id }, Cmd.none )

                        -- MOVE
                        ( IGear id, Interact.Dragged oldPos newPos _, _ ) ->
                            let
                                gearUp =
                                    Gear.update <| Gear.Move <| Vec.sub newPos oldPos
                            in
                            ( D
                                { doc
                                    | dragging = Moving
                                    , data = updateGearsGrouping doc.data <| Coll.update id gearUp
                                }
                            , Cmd.none
                            )

                        ( _, Interact.DragEnded _, Moving ) ->
                            ( D { doc | dragging = NoDrag, data = Data.do mobile doc.data }, Cmd.none )

                        _ ->
                            ( D doc, Cmd.none )


viewTools : Doc -> Element Msg
viewTools (D doc) =
    row [ width fill, padding 10, spacing 20 ]
        [ Input.radioRow [ spacing 30 ]
            { onChange = ChangedTool
            , options =
                [ Input.option Play <| text "Jeu (W)"
                , Input.option Link <| text "Harmonie (X)"
                , Input.option Edit <| text "Édition (C)"
                ]
            , selected = Just doc.tool
            , label = Input.labelHidden "Outils"
            }
        , Input.text [ centerX ]
            { label = Input.labelHidden "Nom du fichier"
            , text = Data.getName doc.data
            , placeholder = Just <| Input.placeholder [] <| text "nom-a-sauvegarder"
            , onChange = EnteredFileName
            }
        , Input.button
            [ centerX
            , Font.color <|
                if Data.isSaved doc.data then
                    rgb 0 0 0

                else
                    rgb 0 1 1
            ]
            { label = text "Sauvegarder"
            , onPress = Just Save
            }
        , Input.button [ alignRight ]
            { label = text "Undo"
            , onPress =
                if Data.canUndo doc.data then
                    Just Undo

                else
                    Nothing
            }
        , Input.button []
            { label = text "Redo"
            , onPress =
                if Data.canRedo doc.data then
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
            Data.current doc.data

        getMod : Interact.Interact Interactable -> Id Gear -> Gear.Mod
        getMod i id =
            if doc.details == DGear id then
                Gear.Selected

            else
                case i of
                    Just ( IGear iid, mode ) ->
                        if iid /= id then
                            Gear.None

                        else
                            case ( doc.tool, mode ) of
                                ( Link, Interact.Hover ) ->
                                    Gear.Resizing

                                ( Edit, Interact.Hover ) ->
                                    Gear.Selectable

                                _ ->
                                    Gear.None

                    Just ( IResizeHandle iid _, mode ) ->
                        if iid /= id then
                            Gear.None

                        else
                            Gear.Resizing

                    _ ->
                        Gear.None
    in
    (List.map (\( id, g ) -> Gear.view ( id, g ) mobile.gears (getMod inter id)) <|
        Coll.toList mobile.gears
    )
        ++ (case doc.dragging of
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

                _ ->
                    []
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
        ++ (case doc.details of
                DHarmolink l _ ->
                    Link.viewSelectedLink mobile.gears l

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
                    , value = Gear.getVolume (Coll.get id (Data.current doc.data).gears)
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
                    { label = text "Changer son"
                    , onPress = Just <| ChangeSound id
                    }
                , Input.button []
                    { onPress = Just <| DeleteGear id
                    , label = text "Supprimer"
                    }
                ]
            ]

        DChangeSound id ->
            [ column [ height fill, Bg.color (rgb 0.5 0.2 0), Font.color (rgb 1 1 1), spacing 20, padding 10 ]
                [ text <| Gear.toUID id
                , text "Choisir un son chargé"
                , Input.button []
                    { label = text "Annuler"
                    , onPress = Just <| ChangeSoundCanceled id
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
                                    { text = String.fromInt fract.num
                                    , onChange = EnteredFract True
                                    , label = Input.labelHidden "Numerator"
                                    , placeholder = Nothing
                                    }
                                , text "/"
                                , Input.text [ Font.color (rgb 0 0 0) ]
                                    { text = String.fromInt fract.den
                                    , onChange = EnteredFract False
                                    , label = Input.labelHidden "Denominator"
                                    , placeholder = Nothing
                                    }
                                , Input.button []
                                    { label = text "Change"
                                    , onPress = Just <| AppliedFract l fract
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


doVolumeChange : Id Gear -> Vec2 -> Vec2 -> Float -> Doc -> ( Doc, Cmd msg )
doVolumeChange id oldPos newPos scale (D doc) =
    let
        gears =
            (Data.current doc.data).gears

        oldVolume =
            Gear.getVolume <| Coll.get id gears

        volume =
            oldVolume + (Vec.getY oldPos - Vec.getY newPos) / scale / 100

        newGears =
            Coll.update id (Gear.update <| Gear.ChangeVolume volume) gears
    in
    ( D { doc | data = updateGearsGrouping doc.data <| always newGears }
    , Engine.volumeChanged id volume doc.engine
    )


doResize id oldPos newPos add (D doc) =
    let
        gears =
            (Data.current doc.data).gears

        length =
            Gear.getLengthId id gears

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
    D { doc | data = updateGearsGrouping doc.data <| Gear.resizeFree id newSize }


computeCuts : ( Vec2, Vec2 ) -> Coll Gear -> List Link
computeCuts cut gears =
    Engine.getAllLinks gears
        |> List.filter (Link.cuts cut << Link.toSegment gears)


updateGears : Data Mobile -> (Coll Gear -> Coll Gear) -> Data Mobile
updateGears data f =
    let
        mobile =
            Data.current data
    in
    Data.do { mobile | gears = f mobile.gears } data


updateGearsGrouping : Data Mobile -> (Coll Gear -> Coll Gear) -> Data Mobile
updateGearsGrouping data f =
    let
        mobile =
            Data.current data
    in
    Data.group { mobile | gears = f mobile.gears } data
