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
        { data : UndoList { gears : Coll Gear, refs : Coll Ref }
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
    = PGear ( Id Gear, Gear, Ref )


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
        PGear tuple ->
            E.object
                [ ( "action", E.string <| actionToString action )
                , ( "item", Gear.encoder tuple )
                ]


idToPlay id data =
    case Coll.get id data.gears of
        Nothing ->
            Debug.log
                ("IMPOSSIBLE No gear to play for id " ++ Gear.toUID id)
                Cmd.none

        Just g ->
            case Coll.get (Gear.getRefId g) data.refs of
                Nothing ->
                    Debug.log ("ERROR No Ref for gear " ++ Gear.toUID id) Cmd.none

                Just r ->
                    toEngine <|
                        engineEncoder { playable = PGear ( id, g, r ), action = PlayPause }



-- TODO Don’t really need anything else than id to stop


idToStop id data =
    case Coll.get id data.gears of
        Nothing ->
            Debug.log
                ("IMPOSSIBLE No gear to play for id " ++ Gear.toUID id)
                Cmd.none

        Just g ->
            case Coll.get (Gear.getRefId g) data.refs of
                Nothing ->
                    Debug.log ("ERROR No Ref for gear " ++ Gear.toUID id) Cmd.none

                Just r ->
                    toEngine <|
                        engineEncoder { playable = PGear ( id, g, r ), action = StopReset }


new : Doc
new =
    D
        { data = Undo.fresh { gears = Coll.empty, refs = Coll.empty }
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

        ( tmpRefs, refId ) =
            Coll.reserve doc.data.present.refs

        ( g, r ) =
            Gear.fromSound sound pos refId
    in
    ( D
        { doc
            | data =
                undoNew doc.data <|
                    \d ->
                        { d
                            | gears = Coll.insert g d.gears
                            , refs = Coll.fillReserved refId r tmpRefs
                        }
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
            ( D { doc | playing = id :: doc.playing }, idToPlay id doc.data.present )

        StopGear id ->
            ( D { doc | playing = List.filter (\el -> el /= id) doc.playing }
            , idToStop id doc.data.present
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
                    | data = undoNew doc.data <| \d -> { d | gears = Coll.remove id d.gears }
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
            ( D
                { doc
                    | data = undoNew doc.data <| \d -> { d | gears = Coll.update id (Gear.update subMsg) d.gears }
                    , playing = List.filter (\el -> el /= id) doc.playing
                }
            , idToStop id doc.data.present
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
                            ( D { doc | playing = playing }, idToPlay id doc.data.present )

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
                        case mode of
                            Interact.Hover ->
                                Gear.Hovered

                            Interact.Click ->
                                Gear.Clicked

                            Interact.Drag ->
                                Gear.Dragged

        getRef : Gear -> Coll Ref -> Ref
        getRef g refs =
            case Coll.get (Gear.getRefId g) refs of
                Nothing ->
                    Debug.log "ERROR No ref found in view" Gear.defaultRef

                Just r ->
                    r
    in
    (List.map (\( id, g ) -> Gear.view ( id, g, getRef g doc.data.present.refs ) (getMod inter id)) <|
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
                            case Coll.get (Gear.getRefId g) doc.data.present.refs of
                                Nothing ->
                                    Debug.log ("ERROR No ref for " ++ Gear.toUID id) []

                                Just r ->
                                    let
                                        l =
                                            Gear.getLength ( g, r )

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
