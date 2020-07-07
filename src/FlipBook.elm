module FlipBook exposing (..)

import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Align(..), ClipPath(..), Length(..), MeetOrSlice(..), Opacity(..))


path =
    "./images/"


class =
    "flip"


type alias Images =
    List String


make =
    [ "1.jpg", "2.jpg" ]


view : Images -> Float -> Svg msg
view urls l =
    S.svg [ SA.class [ class ], SA.clipPath <| ClipPathFunc <| "circle(" ++ String.fromFloat (l / 2) ++ ")" ] <|
        (List.map
            (\url ->
                S.image
                    [ SA.xlinkHref (path ++ url)
                    , SA.opacity <| Opacity 0
                    , SA.width <| Num l
                    , SA.x <| Num <| -l / 2
                    , SA.y <| Num <| -l / 2
                    , SA.preserveAspectRatio AlignNone Meet
                    ]
                    []
            )
         <|
            List.reverse <|
                List.drop 1 urls
        )
            ++ (case List.head urls of
                    Just url ->
                        [ S.image
                            [ SA.xlinkHref (path ++ url)
                            , SA.width <| Num l
                            , SA.x <| Num <| -l / 2
                            , SA.y <| Num <| -l / 2
                            , SA.preserveAspectRatio AlignNone Meet
                            ]
                            []
                        ]

                    Nothing ->
                        []
               )
