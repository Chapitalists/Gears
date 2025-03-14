module Utils.Utils exposing (..)

import Color
import Element
import Html
import Html.Attributes
import Math.Vector2 exposing (Vec2, getX, getY)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Length(..), Opacity(..), Transform(..))


type alias Size =
    { width : Int, height : Int }


htmlId : String -> Element.Attribute msg
htmlId =
    Element.htmlAttribute << Html.Attributes.id


unmaybeMap : Maybe from -> to -> (from -> to) -> to
unmaybeMap may default map =
    case may of
        Nothing ->
            default

        Just a ->
            map a


toggleListElement : el -> List el -> List el
toggleListElement el l =
    if List.member el l then
        List.filter ((/=) el) l

    else
        el :: l



-- TODO not good, name cannot be specified usefully (Wheel.NoMod, Wheel.Selectable)


type WheelMod
    = None
    | Selectable
    | Selected Bool -- First selected



--| Resizing


type alias WheelStyle =
    { mod : WheelMod

    --, motor : Bool
    --, dashed : Bool
    , weaving : Bool

    --, baseColor : Maybe Float
    , named : Maybe String
    }


defaultStyle : WheelStyle
defaultStyle =
    { mod = None
    , weaving = False
    , named = Nothing
    }



--FOR Data.Pupil if it is used


drawWheel :
    Vec2
    -> Float
    -> Float
    -> Color.Color
    -> WheelStyle
    -> String
    -> List (Html.Attribute msg)
    -> List (Svg msg)
    -> List (Svg msg)
    -> Svg msg
drawWheel pos dur startPercent color style uid attrs els rotEls =
    let
        tickH =
            dur / 15

        tickW =
            dur / 30
    in
    S.g
        (SA.transform [ Translate (getX pos) (getY pos) ]
            :: (if style.weaving then
                    [ SA.opacity <| Opacity 0.5 ]

                else
                    []
               )
        )
    <|
        (case style.named of
            Just name ->
                [ S.text_
                    [ SA.x <| Num 0
                    , SA.y <| Num -(dur * 3 / 4)
                    , SA.fontSize <| Num (dur / 2)
                    , SA.textAnchor AnchorMiddle
                    , SA.stroke Color.white
                    , SA.strokeWidth <| Num (tickW / 4)
                    ]
                    [ text name ]
                ]

            Nothing ->
                [ S.text_ [] [] ]
         -- Because rotating g cannot be Keyed in TypedSvg, trick to prevent recreation
        )
            ++ [ S.g
                    -- rotation and drag
                    (if String.isEmpty uid then
                        []

                     else
                        Html.Attributes.id uid :: attrs
                    )
                    ([ S.circle
                        [ SA.cx <| Num 0
                        , SA.cy <| Num 0
                        , SA.r <| Num (dur / 2)
                        , SA.stroke Color.black
                        , SA.strokeWidth <|
                            Num <|
                                if style.mod == Selectable then
                                    tickW * 2

                                else
                                    tickW
                        , SA.fill <| Fill color
                        ]
                        []
                     , S.rect
                        [ SA.width <| Num tickW
                        , SA.height <| Num tickH
                        , SA.x <| Num (tickW / -2)
                        , SA.y <| Num (tickH / -2)
                        , SA.transform
                            [ Rotate (startPercent * 360) 0 0
                            , Translate 0 ((dur / -2) - (tickH / 2))
                            ]
                        ]
                        []
                     ]
                        ++ rotEls
                    )

               -- end rotation drag
               ]
            -- No rot part
            ++ (case style.mod of
                    Selected first ->
                        let
                            strW =
                                if first then
                                    tickW

                                else
                                    tickW / 2
                        in
                        [ S.circle
                            [ SA.cx <| Num 0
                            , SA.cy <| Num 0
                            , SA.r <| Num (dur / 2 + tickW * 2)
                            , SA.strokeWidth <| Num strW
                            , SA.stroke Color.black
                            , SA.fill FillNone
                            ]
                            []
                        ]

                    _ ->
                        []
               )
            ++ els
