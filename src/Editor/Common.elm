module Editor.Common exposing (..)

import Coll exposing (Coll, Id)
import Color
import Data.Collar as Collar
import Data.Content as Content exposing (Content)
import Data.Gear as Gear
import Data.Mobile exposing (Geer)
import Data.Wheel as Wheel exposing (Wheel, Wheeled)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Harmony as Harmo
import Html
import Html.Attributes
import Interact
import Math.Vector2 exposing (Vec2, vec2)
import PanSvg
import Sound exposing (Sound)
import Svg
import TypedSvg as S


svgId : String
svgId =
    "svg"


packId : String
packId =
    "packSvg"


type alias CommonModel =
    { edit : List Identifier
    , pack : Coll Packed
    , packVisible : Bool
    , packSvg : PanSvg.Model
    , packScale : Float
    , dragging : Maybe Packed
    , initPos : Maybe Vec2 -- TODO could be in Dragging type
    }


typeString : String
typeString =
    "packed"


toUID : Id Packed -> String
toUID id =
    typeString ++ "-" ++ Coll.idToString id


type alias Packed =
    { wheel : Wheel
    , length : Float
    , pos : Vec2
    }


defaultPacked =
    { wheel = Wheel.default
    , length = 0
    , pos = vec2 0 0
    }


type Identifier
    = G (Id Geer)
    | B Int
    | P (Id Packed)


getNameFromContent : Identifier -> Content Wheel -> String
getNameFromContent id c =
    Maybe.withDefault "PROBLEM" <|
        Maybe.map
            (\w ->
                if String.isEmpty w.name then
                    case id of
                        B i ->
                            Collar.toUID i

                        G i ->
                            Gear.toUID i

                        P _ ->
                            ""

                else
                    w.name
            )
        <|
            getWheelFromContent id c


getWheelFromContent : Identifier -> Content Wheel -> Maybe Wheel
getWheelFromContent id c =
    case ( id, c ) of
        ( B i, Content.C collar ) ->
            Just (Collar.get i collar).wheel

        ( G i, Content.M mobile ) ->
            Just (Coll.get i mobile.gears).wheel

        _ ->
            Debug.log "IMPOSSIBLE Wrong Identifier to get from Content" Nothing


getLengthFromContent : Identifier -> Content Wheel -> Maybe Float
getLengthFromContent id c =
    case ( id, c ) of
        ( B i, Content.C collar ) ->
            Just (Collar.get i collar).length

        ( G i, Content.M mobile ) ->
            Just <| Harmo.getLengthId i mobile.gears

        _ ->
            Debug.log "IMPOSSIBLE Wrong Identifier to get length from Content" Nothing


type Interactable
    = ISurface
    | IWheel Identifier
    | IResizeHandle Identifier Bool
    | ISound Sound


type Zone
    = ZSurface
    | ZPack


fromWheelInteractable : Wheel.Interactable Identifier -> Interactable
fromWheelInteractable i =
    case i of
        Wheel.IWheel id ->
            IWheel <| id

        Wheel.IResizeHandle id bool ->
            IResizeHandle id bool


commonInit : Maybe CommonModel -> CommonModel
commonInit may =
    Maybe.withDefault
        { edit = []
        , pack = Coll.empty typeString defaultPacked
        , packVisible = False
        , packSvg = PanSvg.init packId
        , packScale = 0.3
        , dragging = Nothing
        , initPos = Nothing
        }
    <|
        Maybe.map (\model -> { model | edit = [] }) may


type ToUndo
    = Do
    | Group
    | Cancel
    | NOOP


type DocMsg
    = Inside Identifier


type CommonMode
    = Normal
    | Nav
    | ChangeSound Identifier


keyCodeToMode : List ( String, CommonMode )
keyCodeToMode =
    [ ( "KeyV", Nav ) ]


type CommonMsg
    = Delete Identifier
    | ShowPack Bool
    | Pack
    | Unpack (Id Packed)
    | EmptyPack
    | DragTo Packed
    | DragFrom (Id Packed) Vec2
    | InitDrag (Id Packed)
    | PrepareZoom PanSvg.Model
    | PackSvgMsg PanSvg.Msg


commonUpdate : CommonMsg -> CommonModel -> CommonModel
commonUpdate msg model =
    case msg of
        Delete id ->
            { model | edit = List.filter ((/=) id) model.edit }

        ShowPack b ->
            { model | packVisible = b }

        Pack ->
            let
                pack =
                    case model.dragging of
                        Just p ->
                            Coll.insert p model.pack

                        Nothing ->
                            model.pack
            in
            { model | pack = pack, dragging = Nothing }

        Unpack id ->
            { model | pack = Coll.remove id model.pack }

        EmptyPack ->
            { model | pack = Coll.empty typeString defaultPacked }

        DragTo p ->
            { model | dragging = Just p }

        DragFrom id pos ->
            { model
                | pack = Coll.update id (\p -> { p | pos = pos }) model.pack
                , initPos = Just <| Maybe.withDefault (Coll.get id model.pack).pos model.initPos
            }

        InitDrag id ->
            case model.initPos of
                Just pos ->
                    { model
                        | pack = Coll.update id (\p -> { p | pos = pos }) model.pack
                        , initPos = Nothing
                    }

                Nothing ->
                    model

        PrepareZoom parent ->
            if Coll.isEmpty model.pack then
                commonUpdate
                    (PackSvgMsg <| PanSvg.SetSmallestSize <| parent.viewPos.smallestSize / model.packScale / 4)
                    model

            else
                model

        PackSvgMsg subMsg ->
            { model | packSvg = PanSvg.update subMsg model.packSvg }


viewDetailsColumn : List (Element msg) -> Element msg
viewDetailsColumn =
    column
        [ height fill
        , Bg.color (rgb 0.5 0.5 0.5)
        , Font.color (rgb 1 1 1)
        , Font.size 16
        , spacing 20
        , padding 10
        ]


viewDetailChangingSound : Identifier -> Content Wheel -> msg -> List (Element msg)
viewDetailChangingSound id c msg =
    [ column [ height fill, Bg.color (rgb 0.5 0.2 0), Font.color (rgb 1 1 1), spacing 20, padding 10 ] <|
        [ text <| getNameFromContent id c
        , text "Choisir un son chargé"
        , Input.button []
            { label = text "Annuler"
            , onPress = Just msg
            }
        ]
    ]


viewNameInput : Wheeled x -> String -> (String -> msg) -> Element msg
viewNameInput w placeHolder msgF =
    Input.text [ Font.color (rgb 0 0 0) ]
        { label = Input.labelAbove [] <| text "Roue :"
        , text = w.wheel.name
        , placeholder = Just <| Input.placeholder [] <| text <| placeHolder
        , onChange = msgF
        }


viewContentButton : Wheeled x -> msg -> Element msg
viewContentButton w msg =
    case Wheel.getContent w of
        Content.S s ->
            text <| Sound.toString s

        Content.M _ ->
            Input.button []
                { label = text "Voir Mobile"
                , onPress = Just msg
                }

        Content.C _ ->
            Input.button []
                { label = text "Voir Collier"
                , onPress = Just msg
                }


viewVolumeSlider : Wheeled x -> (Float -> msg) -> Element msg
viewVolumeSlider w msgF =
    Input.slider []
        { label = Input.labelAbove [] <| text "Volume"
        , onChange = msgF
        , value = w.wheel.volume
        , min = 0
        , max = 1
        , step = Just 0.01
        , thumb = Input.defaultThumb
        }


viewChangeContent : msg -> Element msg
viewChangeContent msg =
    Input.button []
        { label = text "Changer son"
        , onPress = Just msg
        }


viewResizeToInsideLength : msg -> Element msg
viewResizeToInsideLength msg =
    Input.button []
        { label = text "Taille Originale"
        , onPress = Just msg
        }


viewDeleteButton : msg -> Element msg
viewDeleteButton msg =
    Input.button []
        { onPress = Just msg
        , label = text "Supprimer"
        }


viewPackButtons : CommonModel -> List (Element CommonMsg)
viewPackButtons model =
    [ Input.button []
        { label =
            text <|
                if model.packVisible then
                    "Fermer le sac"

                else
                    "Ouvrir le sac"
        , onPress = Just <| ShowPack <| not model.packVisible
        }
    , Input.button []
        { label = text "Vider son sac"
        , onPress = Just <| EmptyPack
        }
    ]


viewPack :
    CommonModel
    -> List (Html.Attribute msg)
    -> (CommonMsg -> msg)
    -> (Interact.Msg Interactable zone -> msg)
    -> Element msg
viewPack model events wrapCommon wrapInteract =
    if model.packVisible then
        el
            ([ Border.color <| rgb 0 0 0
             , Border.width 4
             , Bg.color <| rgb 1 1 1
             , alignBottom
             , alignRight
             ]
                ++ (List.map Element.htmlAttribute <|
                        (Html.Attributes.style "height" <| (String.fromFloat <| model.packScale * 100) ++ "%")
                            :: (Html.Attributes.style "width" <| (String.fromFloat <| model.packScale * 100) ++ "%")
                            :: events
                   )
            )
        <|
            html <|
                S.svg (List.map (Html.Attributes.map (wrapCommon << PackSvgMsg)) <| PanSvg.svgAttributes model.packSvg) <|
                    List.map
                        (\( id, p ) ->
                            Svg.map (wrapInteract << Interact.map fromWheelInteractable) <|
                                Wheel.view p.wheel p.pos p.length Wheel.defaultStyle (P id) <|
                                    toUID id
                        )
                        (Coll.toList model.pack)
                        ++ (case model.dragging of
                                Just { pos, length, wheel } ->
                                    [ Wheel.drawSimple wheel pos length ]

                                Nothing ->
                                    []
                           )

    else
        Element.none


interactNav : Interact.Event Interactable Zone -> Content Wheel -> Maybe DocMsg
interactNav event content =
    case ( event.item, event.action ) of
        ( IWheel id, Interact.Clicked _ ) ->
            case Maybe.map Wheel.getWheelContent <| getWheelFromContent id content of
                Just (Content.M _) ->
                    Just <| Inside id

                Just (Content.C _) ->
                    Just <| Inside id

                _ ->
                    Nothing

        _ ->
            Nothing


interactSelectEdit : Interact.Event Interactable Zone -> CommonModel -> CommonModel
interactSelectEdit event model =
    case ( event.item, event.action ) of
        ( IWheel id, Interact.Clicked ( _, False, False ) ) ->
            { model | edit = [ id ] }

        ( IWheel id, Interact.Clicked _ ) ->
            { model | edit = id :: model.edit }

        _ ->
            model
