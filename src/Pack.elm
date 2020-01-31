module Pack exposing (..)

import Coll exposing (Coll, Id)
import Data.Wheel as Wheel exposing (Wheel)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Input as Input
import Html
import Html.Attributes
import Interact
import Math.Vector2 exposing (Vec2, vec2)
import PanSvg
import Svg
import TypedSvg as S


type alias Pack =
    { content : Maybe Wheel
    , wheels : Coll Packed
    , visible : Bool
    , svg : PanSvg.Model
    , scale : Float
    , dragging : Maybe Packed
    , initPos : Maybe Vec2 -- TODO could be in Dragging type
    }


typeString : String
typeString =
    "packed"


toUID : Id Packed -> String
toUID id =
    typeString ++ "-" ++ Coll.idToString id


svgId : String
svgId =
    "packSvg"


type alias Packed =
    { wheel : Wheel
    , length : Float
    , pos : Vec2
    }


defaultPacked : Packed
defaultPacked =
    { wheel = Wheel.default
    , length = 0
    , pos = vec2 0 0
    }


init : Pack
init =
    { content = Nothing
    , wheels = Coll.empty typeString defaultPacked
    , visible = False
    , svg = PanSvg.init svgId
    , scale = 0.3
    , dragging = Nothing
    , initPos = Nothing
    }


type Msg
    = TogglePack
    | Packontent Wheel
    | PackIt
    | Unpack (Id Packed)
    | EmptyPack
    | DragTo (Maybe Packed)
    | DragFrom (Id Packed) Vec2
    | InitDrag (Id Packed)
    | PrepareZoom PanSvg.Model
    | SvgMsg PanSvg.Msg


update : Msg -> Pack -> Pack
update msg pack =
    case msg of
        TogglePack ->
            { pack | visible = not pack.visible }

        Packontent w ->
            { pack | content = Just w }

        PackIt ->
            let
                wheels =
                    case pack.dragging of
                        Just p ->
                            Coll.insert p pack.wheels

                        Nothing ->
                            pack.wheels
            in
            { pack | wheels = wheels, dragging = Nothing }

        Unpack id ->
            { pack | wheels = Coll.remove id pack.wheels }

        EmptyPack ->
            { pack | wheels = Coll.empty typeString defaultPacked }

        DragTo p ->
            { pack | dragging = p }

        DragFrom id pos ->
            { pack
                | wheels = Coll.update id (\p -> { p | pos = pos }) pack.wheels
                , initPos = Just <| Maybe.withDefault (Coll.get id pack.wheels).pos pack.initPos
            }

        InitDrag id ->
            case pack.initPos of
                Just pos ->
                    { pack
                        | wheels = Coll.update id (\p -> { p | pos = pos }) pack.wheels
                        , initPos = Nothing
                    }

                Nothing ->
                    pack

        PrepareZoom parent ->
            if Coll.isEmpty pack.wheels then
                update
                    (SvgMsg <| PanSvg.SetSmallestSize <| parent.viewPos.smallestSize / pack.scale / 4)
                    pack

            else
                pack

        SvgMsg subMsg ->
            { pack | svg = PanSvg.update subMsg pack.svg }


viewPackButtons : Pack -> List (Element Msg)
viewPackButtons pack =
    [ Input.button []
        { label =
            text <|
                if pack.visible then
                    "Fermer le sac"

                else
                    "Ouvrir le sac"
        , onPress = Just TogglePack
        }
    , Input.button []
        { label = text "Vider son sac"
        , onPress = Just EmptyPack
        }
    ]


view :
    Pack
    -> List (Html.Attribute msg)
    -> (Msg -> msg)
    -> (Id Packed -> inter)
    -> (Interact.Msg inter zone -> msg)
    -> Element msg
view pack events wrap interactable wrapInteract =
    if pack.visible then
        el
            ([ Border.color <| rgb 0 0 0
             , Border.width 4
             , Bg.color <| rgb 1 1 1
             , alignBottom
             , alignRight
             ]
                ++ (List.map Element.htmlAttribute <|
                        (Html.Attributes.style "height" <| (String.fromFloat <| pack.scale * 100) ++ "%")
                            :: (Html.Attributes.style "width" <| (String.fromFloat <| pack.scale * 100) ++ "%")
                            :: events
                   )
            )
        <|
            html <|
                S.svg (List.map (Html.Attributes.map (wrap << SvgMsg)) <| PanSvg.svgAttributes pack.svg) <|
                    List.map
                        (\( id, p ) ->
                            Svg.map wrapInteract <|
                                Wheel.view
                                    p.wheel
                                    p.pos
                                    p.length
                                    Wheel.defaultStyle
                                    (Just <| always <| interactable id)
                                    Nothing
                                    (toUID id)
                        )
                        (Coll.toList pack.wheels)
                        ++ (case pack.dragging of
                                Just { pos, length, wheel } ->
                                    [ Svg.map wrapInteract <|
                                        Wheel.view wheel pos length Wheel.defaultStyle Nothing Nothing ""
                                    ]

                                Nothing ->
                                    []
                           )

    else
        Element.none
