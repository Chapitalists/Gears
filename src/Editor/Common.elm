module Editor.Common exposing (..)

import Coll exposing (Id)
import Data.Collar as Collar
import Data.Content as Content exposing (Content)
import Data.Gear as Gear
import Data.Mobile exposing (Geer)
import Data.Wheel as Wheel exposing (Wheel, Wheeled)
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Harmony as Harmo
import Interact
import Sound


type alias CommonModel =
    { edit : Maybe Identifier
    , pack : Maybe ( Wheel, Float )
    }


type Identifier
    = G (Id Geer)
    | B Int


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


fromWheelInteractable : Wheel.Interactable Identifier -> Interactable
fromWheelInteractable i =
    case i of
        Wheel.IWheel id ->
            IWheel <| id

        Wheel.IResizeHandle id bool ->
            IResizeHandle id bool


commonInit : Maybe CommonModel -> CommonModel
commonInit may =
    { edit = Nothing
    , pack = Maybe.withDefault Nothing <| Maybe.map .pack may
    }


type ToUndo
    = Do
    | Group
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
    | Pack (Content Wheel)
    | EmptyPack


commonUpdate : CommonMsg -> CommonModel -> CommonModel
commonUpdate msg model =
    case msg of
        Delete id ->
            if model.edit == Just id then
                { model | edit = Nothing }

            else
                model

        Pack content ->
            { model
                | pack =
                    model.edit
                        |> Maybe.andThen
                            (\id ->
                                Maybe.map2 Tuple.pair
                                    (getWheelFromContent id content)
                                <|
                                    Debug.log "length" (getLengthFromContent (Debug.log "id" id) <| Debug.log "contont" content)
                            )
            }

        EmptyPack ->
            { model | pack = Nothing }


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


viewPack : CommonModel -> msg -> (( Wheel, Float ) -> msg) -> List (Element msg)
viewPack model packMsg unpackMsg =
    Input.button []
        { onPress = Just packMsg
        , label = text "Copier"
        }
        :: (case model.pack of
                Nothing ->
                    []

                Just ( w, l ) ->
                    [ Input.button []
                        { label = text <| "Coller " ++ w.name
                        , onPress = Just <| unpackMsg ( w, l )
                        }
                    ]
           )


interactNav : Interact.Event Interactable -> Content Wheel -> Maybe DocMsg
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


interactSelectEdit : Interact.Event Interactable -> CommonModel -> CommonModel
interactSelectEdit event model =
    case ( event.item, event.action ) of
        ( IWheel id, Interact.Clicked _ ) ->
            { model | edit = Just id }

        _ ->
            model
