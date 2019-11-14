port module Doc exposing (..)

import Coll exposing (Coll, Id)
import Color
import Element exposing (..)
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


port toEngine : E.Value -> Cmd msg


type Doc
    = D
        { gears : Coll Gear
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


new : Doc
new =
    D
        { gears = Coll.empty
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
            | gears = Coll.insert (Gear.fromSound sound pos) doc.gears
        }
    , pos
    )


type Msg
    = ChangedTool Tool
    | DeleteGear (Id Gear)
    | GearMsg ( Id Gear, Gear.Msg )
    | InteractEvent (Interact.Event String)


update : Msg -> Doc -> ( Doc, Cmd msg )
update msg (D doc) =
    case msg of
        ChangedTool tool ->
            ( D { doc | tool = tool }, Cmd.none )

        DeleteGear id ->
            let
                details =
                    case doc.details of
                        Nothing ->
                            Nothing

                        Just d ->
                            if d == id then
                                Debug.log "empty details" Nothing

                            else
                                doc.details
            in
            ( D { doc | gears = Coll.remove id doc.gears, details = details }, Cmd.none )

        GearMsg ( id, subMsg ) ->
            let
                cmd =
                    case subMsg of
                        Gear.Stop ->
                            case Coll.get id doc.gears of
                                Nothing ->
                                    Debug.log ("IMPOSSIBLE No gear to stop for id " ++ Gear.toUID id) Cmd.none

                                Just g ->
                                    toEngine <| engineEncoder { action = StopReset, playable = PGear ( id, g ) }

                        _ ->
                            Cmd.none
            in
            ( D { doc | gears = Coll.update id (Gear.update subMsg) doc.gears }, cmd )

        InteractEvent event ->
            case ( doc.tool, event ) of
                ( Play, Interact.Clicked uid ) ->
                    case interactableFromUID uid of
                        IGear id ->
                            let
                                cmd =
                                    case Coll.get id doc.gears of
                                        Nothing ->
                                            Debug.log
                                                ("IMPOSSIBLE No gear to play for id " ++ Gear.toUID id)
                                                Cmd.none

                                        Just g ->
                                            toEngine <|
                                                engineEncoder { playable = PGear ( id, g ), action = PlayPause }
                            in
                            ( D { doc | gears = Coll.update id (Gear.update Gear.Play) doc.gears }, cmd )

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
                            ( D
                                { doc
                                    | gears = Coll.update id (Gear.update <| Gear.Move <| Vec.sub newPos oldPos) doc.gears
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( D doc, Cmd.none )

                _ ->
                    ( D doc, Cmd.none )


viewTools : Doc -> Element Msg
viewTools (D doc) =
    Input.radioRow []
        { onChange = ChangedTool
        , options =
            [ Input.option Play <| text "Jouer"
            , Input.option Edit <| text "Éditer"
            , Input.option Link <| text "Lier"
            ]
        , selected = Just doc.tool
        , label = Input.labelHidden "Outils"
        }


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
        Coll.toList doc.gears
    )
        ++ (case doc.futureLink of
                Nothing ->
                    []

                Just ( id, pos ) ->
                    case Coll.get id doc.gears of
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
            case Coll.get id doc.gears of
                Nothing ->
                    Debug.log ("IMPOSSIBLE No gear for details of " ++ Gear.toUID id) []

                Just g ->
                    [ column []
                        [ Input.button []
                            { label = text "x2"
                            , onPress = Just <| GearMsg ( id, Gear.ResizeFract <| Fract.integer 2 )
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
