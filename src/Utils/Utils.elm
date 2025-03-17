module Utils.Utils exposing (..)

import Color
import Element
import Html
import Html.Attributes
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


type alias WheelStyle =
    { selected : Bool
    , named : Maybe String
    }


defaultStyle : WheelStyle
defaultStyle =
    { selected = False
    , named = Nothing
    }


drawWheel :
    Float
    -> Maybe Color.Color
    -> WheelStyle
    -> String
    -> List (Html.Attribute msg)
    -> List (Svg msg)
    -> List (Svg msg)
    -> Svg msg
drawWheel dur mayColor style uid attrs els rotEls =
    let
        strokeWidth =
            dur / 30
    in
    S.g [] <|
        (case style.named of
            Just name ->
                [ S.text_
                    [ SA.x <| Num 0
                    , SA.y <| Num -(dur * 3 / 4)
                    , SA.fontSize <| Num (dur / 2)
                    , SA.textAnchor AnchorMiddle
                    , SA.stroke Color.white
                    , SA.strokeWidth <| Num (strokeWidth / 4)
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
                        , SA.strokeWidth <| Num strokeWidth
                        , SA.fill <| unmaybeMap mayColor FillNone Fill
                        ]
                        []
                     ]
                        ++ rotEls
                    )

               -- end rotation drag
               ]
            -- No rot part
            ++ (if style.selected then
                    [ S.circle
                        [ SA.cx <| Num 0
                        , SA.cy <| Num 0
                        , SA.r <| Num (dur / 2 + strokeWidth * 2)
                        , SA.strokeWidth <| Num <| strokeWidth / 2
                        , SA.stroke Color.black
                        , SA.fill FillNone
                        ]
                        []
                    ]

                else
                    []
               )
            ++ els
