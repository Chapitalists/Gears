port module Tools.PanSvg exposing
    ( Direction(..)
    , FloatSize
    , Msg(..)
    , PanSvg
    , centerZoom
    , getScale
    , init
    , mapIn
    , mapOut
    , newSVGSize
    , sizeDecoder
    , svgAttributes
    , update
    )

import Html.Attributes
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Tools.Utils exposing (Size)
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types


port newSVGSize : (D.Value -> msg) -> Sub msg



-- CONSTANTS


zoomFactor =
    8


type PanSvg
    = Model Internals


type alias Internals =
    { svgSize : FloatSize
    , viewPos : ViewPos
    , id : String
    }


type alias FloatSize =
    { width : Float
    , height : Float
    }


init : String -> Size -> Vec2 -> Float -> PanSvg
init id svgSize pos size =
    Model
        { svgSize = FloatSize (toFloat svgSize.width) (toFloat svgSize.height)
        , viewPos = ViewPos pos <| size * zoomFactor
        , id = id
        }


sizeDecoder : D.Decoder FloatSize
sizeDecoder =
    D.map2 FloatSize (D.field "width" D.float) (D.field "height" D.float)


type alias ViewPos =
    { c : Vec2, smallestSize : Float }


getScale : PanSvg -> Float
getScale (Model { viewPos, svgSize }) =
    viewPos.smallestSize / min svgSize.height svgSize.width


mapIn : Vec2 -> PanSvg -> Vec2
mapIn pos (Model { viewPos, svgSize }) =
    Vec.add
        viewPos.c
    <|
        Vec.scale
            (viewPos.smallestSize / min svgSize.height svgSize.width)
        <|
            Vec.sub
                pos
                (vec2 (svgSize.width / 2) (svgSize.height / 2))


mapOut : Vec2 -> PanSvg -> Vec2
mapOut pos (Model { viewPos, svgSize }) =
    Vec.sub pos viewPos.c
        |> Vec.scale (min svgSize.height svgSize.width / viewPos.smallestSize)
        |> Vec.add (vec2 (svgSize.width / 2) (svgSize.height / 2))


centerZoom : ( Vec2, Float ) -> PanSvg -> PanSvg
centerZoom ( pos, size ) (Model model) =
    Model { model | viewPos = ViewPos pos <| size * zoomFactor }


type Msg
    = ScaleSize Float FloatSize
    | NewSize Size
    | SetSmallestSize Float
    | ZoomPoint Float ( Float, Float )
    | Pan Direction
    | Move Vec2


type Direction
    = Left
    | Right
    | Up
    | Down


update : Msg -> PanSvg -> PanSvg
update msg (Model model) =
    (case msg of
        ScaleSize scale size ->
            { model | svgSize = { width = size.width * scale, height = size.height * scale } }

        NewSize intSize ->
            { model
                | svgSize =
                    { width = toFloat intSize.width
                    , height = toFloat intSize.height
                    }
            }

        SetSmallestSize f ->
            { model | viewPos = ViewPos model.viewPos.c f }

        ZoomPoint f ( x, y ) ->
            let
                vp =
                    model.viewPos

                factor =
                    clamp 0.01 2 <| 1 + f / 1000

                p =
                    Vec.sub (mapIn (vec2 x y) (Model model)) vp.c

                nS =
                    vp.smallestSize * factor

                scale =
                    nS / vp.smallestSize - 1

                nC =
                    Vec.sub vp.c <| Vec.scale scale p
            in
            { model | viewPos = { c = nC, smallestSize = nS } }

        Pan dir ->
            let
                viewPos =
                    model.viewPos

                d =
                    viewPos.smallestSize / 50
            in
            { model
                | viewPos =
                    { viewPos
                        | c =
                            Vec.add model.viewPos.c <|
                                case dir of
                                    Left ->
                                        vec2 -d 0

                                    Right ->
                                        vec2 d 0

                                    Up ->
                                        vec2 0 -d

                                    Down ->
                                        vec2 0 d
                    }
            }

        Move d ->
            let
                viewPos =
                    model.viewPos
            in
            { model | viewPos = { viewPos | c = Vec.sub viewPos.c d } }
    )
        |> Model


svgAttributes : PanSvg -> List (Svg.Attribute Msg)
svgAttributes (Model model) =
    [ computeViewBox model
    , Wheel.onWheel (\e -> ZoomPoint e.deltaY e.mouseEvent.offsetPos)
    , Html.Attributes.id model.id
    , Svg.attribute "width" "100%"
    , Svg.attribute "height" "100%"
    , SA.preserveAspectRatio TypedSvg.Types.AlignNone TypedSvg.Types.Meet
    ]


computeViewBox : Internals -> Svg.Attribute Msg
computeViewBox { viewPos, svgSize } =
    if svgSize.height == 0 || svgSize.width == 0 then
        SA.viewBox 0 0 100 100

    else
        let
            landscapeOrientation =
                svgSize.height < svgSize.width

            ratio =
                if landscapeOrientation then
                    svgSize.width / svgSize.height

                else
                    svgSize.height / svgSize.width

            h =
                viewPos.smallestSize

            w =
                h * ratio

            x =
                Vec.getX viewPos.c - w / 2

            y =
                Vec.getY viewPos.c - h / 2
        in
        if landscapeOrientation then
            SA.viewBox x y w h

        else
            SA.viewBox x y h w
