module FlipBook exposing (..)

import Element exposing (..)
import Time
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


type alias FlipBook =
    { urls : Images
    , current : Int
    , interval : Float
    , running : Bool
    }


make =
    [ "1.jpg", "2.jpg" ]


init =
    FlipBook [] 0 200 False


type Msg
    = Play
    | Pause
    | Flip
    | Urls Images


update : Msg -> FlipBook -> FlipBook
update msg model =
    case msg of
        Play ->
            { model | running = True }

        Pause ->
            { model | running = False }

        Flip ->
            { model | current = modBy (List.length model.urls) <| model.current + 1 }

        Urls urls ->
            { model | urls = urls }


subs : FlipBook -> Sub Msg
subs { running, interval } =
    if running then
        Time.every interval <| always Flip

    else
        Sub.none


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


preview : FlipBook -> Int -> Attribute msg
preview { urls } w =
    inFront <|
        row [ width <| px w, spaceEvenly ] <|
            List.map
                (\url ->
                    image [ width <| px <| round <| toFloat w / (toFloat <| List.length urls) ]
                        { src = url, description = "image to decrypt" }
                )
                urls


flip : FlipBook -> Int -> Attribute msg
flip { urls, interval, current } h =
    let
        mayUrl =
            List.head <| List.drop current <| urls
    in
    case mayUrl of
        Just url ->
            inFront <| image [ height <| px <| h - 150 ] { src = url, description = "image n°" ++ String.fromInt current }

        Nothing ->
            inFront none
