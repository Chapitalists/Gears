port module Doc exposing (..)

import Coll exposing (Coll, Id)
import Element exposing (..)
import Element.Input as Input
import Gear exposing (Gear)
import Html.Attributes
import Interact
import Json.Encode as E
import Math.Vector2 exposing (Vec2, vec2)
import Sound exposing (Sound)
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types


port toEngine : E.Value -> Cmd msg


type Doc
    = D { gears : Coll Gear, tool : Tool }


type Tool
    = Edit
    | Play
    | Link


type Interactable
    = IGear (Id Gear)


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
    D { gears = Coll.empty, tool = Play }


getGear : Id Gear -> Doc -> Maybe Gear
getGear id (D { gears }) =
    Coll.get id gears


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
            ( D { doc | gears = Coll.remove id doc.gears }, Cmd.none )

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
            ( D doc, Cmd.none )



-- TODO DODODO


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
    List.map (\( id, g ) -> Gear.view ( id, g ) (getMod inter id)) <|
        Coll.toList doc.gears
