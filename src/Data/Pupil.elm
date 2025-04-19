module Data.Pupil exposing
    ( Pupil
    , fromSound
    , getColor
    , getContent
    , getDuration
    , getEngined
    , scaleDuration
    , view
    )

import Color
import Data.Content as Content exposing (Content)
import Html
import Math.Vector2 exposing (vec2)
import Sound exposing (Sound)
import TypedSvg.Core exposing (Svg)
import Utils.Utils exposing (defaultStyle, drawWheel)


type Pupil
    = Model Internals


type alias Internals =
    { hue : Float -- hue
    , duration : Float -- millis
    , sound : Sound -- WARNING temporary simplification

    --, content : Content a
    , viewContent : Bool
    }


type alias Engined =
    { duration : Float
    , sound : Sound

    --, content : Content
    }


fromSound : Sound -> Float -> Pupil
fromSound sound hue =
    Model
        { hue = hue
        , duration = Sound.length sound
        , sound = sound
        , viewContent = True
        }


getEngined : Pupil -> Engined
getEngined (Model model) =
    { duration = model.duration
    , sound = model.sound

    --, content = model.content
    }


getContent : Pupil -> Content a
getContent (Model model) =
    Content.S model.sound



-- WARNING temporary simplification


getLoopPercents : Internals -> ( Float, Float )
getLoopPercents model =
    Sound.getLoopPercents model.sound


getDuration : Pupil -> Float
getDuration (Model model) =
    model.duration


scaleDuration : Float -> Pupil -> Pupil
scaleDuration scale (Model model) =
    Model
        { model | duration = model.duration * scale }


getColor : Pupil -> Maybe Color.Color
getColor (Model model) =
    Just <| Color.hsl model.hue 1 0.5



-- WARNING temporary simplification
--case model.content of
--    Content.S s ->
--        Sound.getLoopPercents s
--
--    Content.C c ->
--        case c.oneSound of
--            Just one ->
--                ( one.start, one.end )
--
--            _ ->
--                ( 0, 1 )
--
--    _ ->
--        ( 0, 1 )


view :
    Pupil
    -> String
    -> List (Html.Attribute msg)
    -> Svg msg
view (Model model) uid attrs =
    let
        length =
            getDuration (Model model)

        mayColor =
            getColor (Model model)
    in
    drawWheel
        length
        mayColor
        defaultStyle
        uid
        attrs
        []
        []



--(0.85 - 0.35 * w.volume)
--(if viewContent then
--                                case w.content of
--                                    C (Content.C collar) ->
--                                        let
--                                            scale =
--                                                length / Content.getMatriceLength collar
--                                        in
--                                        [ S.g [ SA.transform [ Translate (-length / 2) 0, Scale scale scale ] ] <|
--                                            insideCollarView collar mayWheelInteract uid
--                                        ]
--
--                                    _ ->
--                                        Debug.todo "view Sound or Mobile inside wheel"
--
--                            else
--                            let
--                                    symSize =
--                                        d / 4
--                                in
--                                case w.content of
--                                    C (Content.M _) ->
--                                        [ S.line
--                                            [ SA.x1 <| Num -symSize
--                                            , SA.y1 <| Num -symSize
--                                            , SA.x2 <| Num symSize
--                                            , SA.y2 <| Num symSize
--                                            , SA.stroke Color.grey
--                                            , SA.strokeWidth <| Num tickW
--                                            ]
--                                            []
--                                        , S.line
--                                            [ SA.x1 <| Num -symSize
--                                            , SA.y1 <| Num symSize
--                                            , SA.x2 <| Num symSize
--                                            , SA.y2 <| Num -symSize
--                                            , SA.stroke Color.grey
--                                            , SA.strokeWidth <| Num tickW
--                                            ]
--                                            []
--                                        ]
--
--                                    C (Content.C _) ->
--                                        [ S.line
--                                            [ SA.x1 <| Num -symSize
--                                            , SA.y1 <| Num 0
--                                            , SA.x2 <| Num symSize
--                                            , SA.y2 <| Num 0
--                                            , SA.stroke Color.grey
--                                            , SA.strokeWidth <| Num tickW
--                                            ]
--                                            []
--                                        ]
--
--                                    _ ->
--                                        []
