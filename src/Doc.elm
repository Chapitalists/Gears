port module Doc exposing (..)

import Coll exposing (Coll, Id)
import Color
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Fraction as Fract
import Gear exposing (Gear, Ref)
import Interact
import Json.Encode as E
import Link exposing (Link)
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Length(..), Transform(..))
import UndoList as Undo exposing (UndoList)



-- TODO Engine Module


port toEngine : E.Value -> Cmd msg


type Doc
    = D
        { data : UndoList Mobile
        , playing : List (Id Gear)
        , futureLink : Maybe ( Id Gear, Vec2 )
        , tool : Tool
        , details : Maybe (Id Gear)
        }


type alias Mobile =
    { gears : Coll Gear, motor : Id Gear }


type Tool
    = Edit
    | Play
    | Link


type Interactable
    = IGear (Id Gear)
    | IResizeHandle (Id Gear) Bool
    | INothing


type HasDetails
    = DGear (Id Gear)


type Playable
    = PGear (Id Gear)


type EngineAction
    = PlayPause
    | StopReset


actionToString a =
    case a of
        PlayPause ->
            "playPause"

        StopReset ->
            "stopReset"


engineEncoder : { action : EngineAction, playable : Playable, mobile : Mobile } -> E.Value
engineEncoder { action, playable, mobile } =
    case playable of
        PGear id ->
            E.object
                [ ( "action", E.string <| actionToString action )
                , ( "item", Gear.encoder id mobile.gears )
                ]


new : Doc
new =
    D
        { data = Undo.fresh { gears = Coll.empty, motor = Coll.startId }
        , playing = []
        , futureLink = Nothing
        , tool = Play
        , details = Nothing
        }


interactableFromUID : String -> Interactable
interactableFromUID uid =
    case String.split "-" uid of
        stringType :: int :: _ ->
            if stringType == Gear.stringType then
                IGear (Coll.forgeId int)

            else
                case String.split "." stringType of
                    "resize" :: dir :: strType :: _ ->
                        if strType == Gear.stringType then
                            IResizeHandle (Coll.forgeId int) (dir == "right")

                        else
                            Debug.log ("ERROR Unrecognized UID resize type " ++ strType) INothing

                    _ ->
                        Debug.log ("ERROR Unrecognized UID type " ++ stringType) INothing

        _ ->
            Debug.log ("ERROR Unrecognized UID " ++ uid) INothing


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
    | PlayGear (Id Gear)
    | StopGear (Id Gear)
    | CopyGear (Id Gear)
    | DeleteGear (Id Gear)
    | Undo
    | Redo
    | GearMsg ( Id Gear, Gear.Msg )
    | InteractEvent (Interact.Event String)


update : Msg -> Doc -> ( Doc, Cmd msg )
update msg (D doc) =
    case msg of
        ChangedTool tool ->
            ( D { doc | tool = tool }, Cmd.none )

        PlayGear id ->
            ( D { doc | playing = id :: doc.playing }
            , toEngine <|
                engineEncoder
                    { action = PlayPause
                    , playable = PGear id
                    , mobile = doc.data.present
                    }
            )

        StopGear id ->
            ( D { doc | playing = List.filter (\el -> el /= id) doc.playing }
            , toEngine <|
                engineEncoder
                    { action = StopReset
                    , playable = PGear id
                    , mobile = doc.data.present
                    }
            )

        CopyGear id ->
            ( D { doc | data = undoNew doc.data (\m -> { m | gears = Gear.copy id m.gears }) }, Cmd.none )

        DeleteGear id ->
            case Coll.get id doc.data.present.gears of
                Nothing ->
                    ( D doc, Cmd.none )

                Just g ->
                    case Gear.getMotherId g doc.data.present.gears of
                        Nothing ->
                            Debug.log "TODO delete mother" ( D doc, Cmd.none )

                        Just motherId ->
                            let
                                details =
                                    case doc.details of
                                        Nothing ->
                                            Nothing

                                        Just d ->
                                            if d == id then
                                                Nothing

                                            else
                                                doc.details
                            in
                            ( D
                                { doc
                                    | data =
                                        undoNew doc.data <|
                                            \d ->
                                                { d
                                                    | gears =
                                                        d.gears
                                                            |> Coll.update motherId (Gear.removeFromRefGroup id)
                                                            |> Coll.remove id
                                                }
                                    , playing = List.filter (\el -> el /= id) doc.playing
                                    , details = details
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

        InteractEvent event ->
            case ( doc.tool, event ) of
                ( Play, Interact.Clicked uid ) ->
                    case interactableFromUID uid of
                        IGear id ->
                            update (PlayGear id) (D doc)

                        _ ->
                            ( D doc, Cmd.none )

                ( Edit, Interact.Clicked uid ) ->
                    case interactableFromUID uid of
                        IGear id ->
                            ( D { doc | details = Just id }, Cmd.none )

                        _ ->
                            ( D doc, Cmd.none )

                ( Link, Interact.Dragged uid oldPos newPos ) ->
                    case interactableFromUID uid of
                        IGear id ->
                            ( D { doc | futureLink = Just ( id, newPos ) }, Cmd.none )

                        _ ->
                            ( D doc, Cmd.none )

                ( Link, Interact.DragEnded uid valid ) ->
                    if valid then
                        --TODO
                        ( D { doc | futureLink = Nothing }, Cmd.none )

                    else
                        ( D { doc | futureLink = Nothing }, Cmd.none )

                ( _, Interact.Dragged uid oldPos newPos ) ->
                    case interactableFromUID uid of
                        IGear id ->
                            update (GearMsg <| ( id, Gear.Move <| Vec.sub newPos oldPos )) <| D doc

                        IResizeHandle id add ->
                            case Coll.get id doc.data.present.gears of
                                Nothing ->
                                    Gear.debugGear id "No gear to resize" ( D doc, Cmd.none )

                                Just g ->
                                    let
                                        d =
                                            Vec.getX newPos - Vec.getX oldPos

                                        dd =
                                            if add then
                                                d

                                            else
                                                -d

                                        l =
                                            d + Gear.getLength g doc.data.present.gears
                                    in
                                    ( D
                                        { doc
                                            | data =
                                                undoNew doc.data
                                                    (\m -> { m | gears = Gear.resizeFree id l m.gears })
                                        }
                                    , Cmd.none
                                    )

                        _ ->
                            ( D doc, Cmd.none )

                _ ->
                    ( D doc, Cmd.none )


viewTools : Doc -> Element Msg
viewTools (D doc) =
    row [ width fill, padding 10, spacing 20 ]
        [ Input.radioRow [ spacing 30 ]
            { onChange = ChangedTool
            , options =
                [ Input.option Play <| text "Jouer"
                , Input.option Edit <| text "Éditer"
                , Input.option Link <| text "Lier"
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


viewContent : Doc -> Interact.Interact String -> List (Svg Gear.OutMsg)
viewContent (D doc) inter =
    let
        getMod : Interact.Interact String -> Id Gear -> Gear.Mod
        getMod i id =
            case i of
                Nothing ->
                    Gear.None

                Just ( uid, mode ) ->
                    if uid /= Gear.toUID id then
                        Gear.None

                    else
                        case ( doc.tool, mode ) of
                            ( Edit, Interact.Hover ) ->
                                Gear.Resizable

                            ( _, Interact.Hover ) ->
                                Gear.Hovered

                            ( _, Interact.Click ) ->
                                Gear.Clicked

                            ( _, Interact.Drag ) ->
                                Gear.Dragged
    in
    (List.map (\( id, g ) -> Gear.view ( id, g ) doc.data.present.gears (getMod inter id)) <|
        Coll.toList doc.data.present.gears
    )
        ++ (case doc.futureLink of
                Nothing ->
                    []

                Just ( id, pos ) ->
                    case Coll.get id doc.data.present.gears of
                        Nothing ->
                            Debug.log ("IMPOSSIBLE future link didn’t found gear " ++ Gear.toUID id) []

                        Just g ->
                            [ Link.drawLink
                                ( Gear.getPos g, pos )
                                (Gear.getLength g doc.data.present.gears)
                            ]
           )
        ++ (List.concatMap (Link.view doc.data.present.gears) <|
                List.concatMap Gear.getGearLinks <|
                    Coll.values doc.data.present.gears
           )


viewDetails : Doc -> List (Element Msg)
viewDetails (D doc) =
    case doc.details of
        Nothing ->
            []

        Just id ->
            case Coll.get id doc.data.present.gears of
                Nothing ->
                    Debug.log ("IMPOSSIBLE No gear for details of " ++ Gear.toUID id) []

                Just g ->
                    [ column [ height fill, Bg.color (rgb 0.5 0.5 0.5), Font.color (rgb 1 1 1), spacing 20, padding 10 ]
                        [ text <| Gear.toUID id
                        , Input.button []
                            (if List.member id doc.playing then
                                { label = text "Stop"
                                , onPress = Just <| StopGear id
                                }

                             else
                                { label = text "Play"
                                , onPress = Just <| PlayGear id
                                }
                            )
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


undoNew : UndoList model -> (model -> model) -> UndoList model
undoNew undo action =
    Undo.new (action undo.present) undo


undont : UndoList model -> (model -> model) -> UndoList model
undont undo action =
    { undo | present = action undo.present }
