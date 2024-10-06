module Palette exposing (..)

import Color as C
import Element as E exposing (Element, el, mouseDown, mouseOver)
import Element.Background as Bg
import Element.Border as Border


bgBase =
    ( Opacy 0.8, Grey )


borderBase =
    ( Vivid, Charcoal )


strokeBase =
    2


roundBase =
    10


marginBase =
    10


roundButton : Int -> Bool -> Bool -> Palette -> Element msg -> Element msg
roundButton sizeEl active seled bg =
    let
        sizeIn =
            ceiling <| toFloat sizeEl * sqrt 2.0

        stroke =
            max 3 <|
                ceiling <|
                    toFloat sizeIn
                        / 30

        size =
            sizeIn + stroke * 2

        sizeOut =
            size + stroke * 2

        selFactor =
            if seled then
                1

            else
                0

        tint =
            if active then
                if seled then
                    Vivid

                else
                    Light

            else
                Dark
    in
    el
        [ Border.width 1
        , Border.color (E.rgba 0 0 0 selFactor)
        , mouseDown [ Border.color (E.rgb 0 0 0) ]
        , Border.rounded sizeOut
        ]
        << el
            [ Border.width stroke
            , Border.rounded size
            , Border.color (E.rgba 0 0 0 0)
            , mouseDown [ Border.color (E.rgba 0 0 0 0) ]
            , mouseOver [ Border.color (E.rgb 0 0 0) ]
            ]
        << el
            [ Bg.color <| toEl ( tint, bg )
            , E.width <| E.px sizeIn
            , E.height <| E.px sizeIn
            , Border.color (E.rgb 0 0 0)
            , Border.width stroke
            , Border.rounded sizeIn
            , E.clip
            , E.padding <| (sizeIn - sizeEl) // 2
            ]


icon : Int -> String -> String -> Element msg
icon size desc url =
    el
        [ E.width <| E.px size
        , E.height <| E.px size
        , E.clip
        ]
    <|
        E.image []
            { description = desc -- TODO localize
            , src = url
            }


type Palette
    = Brown
    | Red
    | Orange
    | Green
    | Yellow
    | Purple
    | Blue
    | Grey
    | Charcoal


type Variation
    = Light
    | Dark
    | Vivid
    | Opacy Float


toEl : ( Variation, Palette ) -> E.Color
toEl =
    toCol >> C.toRgba >> E.fromRgb


toCol : ( Variation, Palette ) -> C.Color
toCol ( v, p ) =
    case v of
        Light ->
            case p of
                Brown ->
                    C.lightBrown

                Red ->
                    C.lightRed

                Orange ->
                    C.lightOrange

                Green ->
                    C.lightGreen

                Yellow ->
                    C.lightYellow

                Purple ->
                    C.lightPurple

                Blue ->
                    C.lightBlue

                Grey ->
                    C.lightGrey

                Charcoal ->
                    C.lightCharcoal

        Dark ->
            case p of
                Brown ->
                    C.darkBrown

                Red ->
                    C.darkRed

                Orange ->
                    C.darkOrange

                Green ->
                    C.darkGreen

                Yellow ->
                    C.darkYellow

                Purple ->
                    C.darkPurple

                Blue ->
                    C.darkBlue

                Grey ->
                    C.darkGrey

                Charcoal ->
                    C.darkCharcoal

        Vivid ->
            case p of
                Brown ->
                    C.brown

                Red ->
                    C.red

                Orange ->
                    C.orange

                Green ->
                    C.green

                Yellow ->
                    C.yellow

                Purple ->
                    C.purple

                Blue ->
                    C.blue

                Grey ->
                    C.grey

                Charcoal ->
                    C.charcoal

        Opacy f ->
            let
                rgba =
                    C.toRgba <| toCol ( Vivid, p )
            in
            C.fromRgba { rgba | alpha = f }
