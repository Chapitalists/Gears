port module Doc exposing (..)

import Coll exposing (Coll, Id)
import Color
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Fraction as Fract
import Gear exposing (Gear)
import Interact
import Json.Encode as E
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Length(..), Transform(..))
import UndoList as Undo exposing (UndoList)


port toEngine : E.Value -> Cmd msg


type Doc
    = D
        { gears : UndoList (Coll Gear)
        , playing : List (Id Gear)
        , futureLink : Maybe ( Id Gear, Vec2 )
        , tool : Tool
        , details : Maybe (Id Gear)
        }


type Tool
    = Edit
    | Play
    | Link


type Interactable
    = IGear (Id Gear)
    | INothing


type HasDetails
    = DGear (Id Gear)


type Playable
    = PGear ( Id Gear, Gear )


type EngineAction
    = PlayPause
    | StopReset


actionToString a =
    case a of
        PlayPause ->
            "playPause"

        StopReset ->
            "stopReset"


engineEncoder : { action : EngineAction, playable : Playable } -> E.Value
engineEncoder { action, playable } =
    case playable of
        PGear ( id, g ) ->
            E.object
                [ ( "action", E.string <| actionToString action )
                , ( "item", Gear.encoder ( id, g ) )
                ]


enginePlayGear ( id, mayGear ) =
    case mayGear of
        Nothing ->
            Debug.log
                ("IMPOSSIBLE No gear to play for id " ++ Gear.toUID id)
                Cmd.none

        Just g ->
            toEngine <|
                engineEncoder { playable = PGear ( id, g ), action = PlayPause }


engineStopGear ( id, mayGear ) =
    case mayGear of
        Nothing ->
            Debug.log
                ("IMPOSSIBLE No gear to stop for id " ++ Gear.toUID id)
                Cmd.none

        Just g ->
            toEngine <|
                engineEncoder { playable = PGear ( id, g ), action = StopReset }


new : Doc
new =
    D
        { gears = Undo.fresh Coll.empty
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
            | gears = undoNew doc.gears <| Coll.insert (Gear.fromSound sound pos)
        }
    , pos
    )


type Msg
    = ChangedTool Tool
    | PlayGear (Id Gear)
    | StopGear (Id Gear)
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
            ( D { doc | playing = id :: doc.playing }, enginePlayGear ( id, Coll.get id doc.gears.present ) )

        StopGear id ->
            ( D { doc | playing = List.filter (\el -> el /= id) doc.playing }
            , engineStopGear ( id, Coll.get id doc.gears.present )
            )

        DeleteGear id ->
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
                    | gears = undoNew doc.gears <| Coll.remove id
                    , playing = List.filter (\el -> el /= id) doc.playing
                    , details = details
                }
            , Cmd.none
            )

        Undo ->
            ( D { doc | gears = Undo.undo doc.gears }, Cmd.none )

        Redo ->
            ( D { doc | gears = Undo.redo doc.gears }, Cmd.none )

        GearMsg ( id, subMsg ) ->
            ( D
                { doc
                    | gears = undoNew doc.gears <| Coll.update id (Gear.update subMsg)
                    , playing = List.filter (\el -> el /= id) doc.playing
                }
            , engineStopGear ( id, Coll.get id doc.gears.present )
            )

        InteractEvent event ->
            case ( doc.tool, event ) of
                ( Play, Interact.Clicked uid ) ->
                    case interactableFromUID uid of
                        IGear id ->
                            let
                                playing =
                                    if List.member id doc.playing then
                                        doc.playing

                                    else
                                        id :: doc.playing
                            in
                            ( D { doc | playing = playing }, enginePlayGear ( id, Coll.get id doc.gears.present ) )

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

                ( Link, Interact.DragCanceled ) ->
                    ( D { doc | futureLink = Nothing }, Cmd.none )

                --TODO
                ( Link, Interact.DragEnded ) ->
                    ( D { doc | futureLink = Nothing }, Cmd.none )

                ( _, Interact.Dragged uid oldPos newPos ) ->
                    case interactableFromUID uid of
                        IGear id ->
                            update (GearMsg <| ( id, Gear.Move <| Vec.sub newPos oldPos )) <| D doc

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
                if Undo.hasPast doc.gears then
                    Just Undo

                else
                    Nothing
            }
        , Input.button []
            { label = text "Redo"
            , onPress =
                if Undo.hasFuture doc.gears then
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
                        case mode of
                            Interact.Hover ->
                                Gear.Hovered

                            Interact.Click ->
                                Gear.Clicked

                            Interact.Drag ->
                                Gear.Dragged
    in
    (List.map (\( id, g ) -> Gear.view ( id, g ) (getMod inter id)) <|
        Coll.toList doc.gears.present
    )
        ++ (case doc.futureLink of
                Nothing ->
                    []

                Just ( id, pos ) ->
                    case Coll.get id doc.gears.present of
                        Nothing ->
                            Debug.log ("IMPOSSIBLE future link didn’t found gear " ++ Gear.toUID id) []

                        Just g ->
                            let
                                l =
                                    Gear.getLength g

                                linkW =
                                    l / 30

                                center =
                                    Gear.getPos g
                            in
                            [ S.polyline
                                [ SA.points [ tupleFromVec center, tupleFromVec pos ]
                                , SA.stroke <| Color.brown
                                , SA.strokeWidth <| Num linkW
                                , SA.strokeLinecap TypedSvg.Types.StrokeLinecapRound
                                ]
                                []
                            ]
           )


viewDetails : Doc -> List (Element Msg)
viewDetails (D doc) =
    case doc.details of
        Nothing ->
            []

        Just id ->
            case Coll.get id doc.gears.present of
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


tupleFromVec : Vec2 -> ( Float, Float )
tupleFromVec v =
    ( Vec.getX v, Vec.getY v )


undoNew : UndoList model -> (model -> model) -> UndoList model
undoNew undo action =
    Undo.new (action undo.present) undo


undont : UndoList model -> (model -> model) -> UndoList model
undont undo action =
    { undo | present = action undo.present }
