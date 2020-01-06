module Editor.Common exposing (..)

import Content exposing (Content)
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Harmony as Harmo
import Sound
import Wheel exposing (Wheel, Wheeled)


type ToUndo
    = Do
    | Group
    | NOOP


type CommonMode
    = Normal
    | Nav


keyCodeToMode : List ( String, CommonMode )
keyCodeToMode =
    [ ( "KeyV", Nav ) ]


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


viewDeleteButton : msg -> Element msg
viewDeleteButton msg =
    Input.button []
        { onPress = Just msg
        , label = text "Supprimer"
        }



-- TODO not used in editors but in datas (collar and mobile) -> Data.Common?


getContentLength : Content Wheel -> Float
getContentLength c =
    case c of
        Content.S s ->
            Sound.length s

        Content.M m ->
            Harmo.getLengthId m.motor m.gears

        Content.C col ->
            col.matrice
