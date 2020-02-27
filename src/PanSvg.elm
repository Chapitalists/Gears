port module PanSvg exposing (..)

import Html.Attributes
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Math.Vector2 as Vec exposing (Vec2, vec2)
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types


port newSVGSize : (D.Value -> msg) -> Sub msg


type alias Model =
    { svgSize : Size
    , viewPos : ViewPos
    , id : String
    }


type alias Size =
    { width : Float
    , height : Float
    }


sizeDecoder : D.Decoder Size
sizeDecoder =
    D.map2 Size (D.field "width" D.float) (D.field "height" D.float)


type alias ViewPos =
    { c : Vec2, smallestSize : Float }


getScale : Model -> Float
getScale { viewPos, svgSize } =
    viewPos.smallestSize / min svgSize.height svgSize.width


mapIn : Vec2 -> Model -> Vec2
mapIn pos { viewPos, svgSize } =
    Vec.add
        viewPos.c
    <|
        Vec.scale
            (viewPos.smallestSize / min svgSize.height svgSize.width)
        <|
            Vec.sub
                pos
                (vec2 (svgSize.width / 2) (svgSize.height / 2))


mapOut : Vec2 -> Model -> Vec2
mapOut pos { viewPos, svgSize } =
    Vec.sub pos viewPos.c
        |> Vec.scale (min svgSize.height svgSize.width / viewPos.smallestSize)
        |> Vec.add (vec2 (svgSize.width / 2) (svgSize.height / 2))


centerZoom : ( Vec2, Float ) -> Model -> Model
centerZoom ( pos, size ) model =
    { model | viewPos = ViewPos pos <| size * 8 }


init : String -> Model
init id =
    { svgSize = Size 0 0
    , viewPos = ViewPos (vec2 0 0) 10
    , id = id
    }


type Msg
    = ScaleSize Float Size
    | SetSmallestSize Float
    | ZoomPoint Float ( Float, Float )
    | Pan Direction
    | Move Vec2


type Direction
    = Left
    | Right
    | Up
    | Down


update : Msg -> Model -> Model
update msg model =
    case msg of
        ScaleSize scale size ->
            { model | svgSize = { width = size.width * scale, height = size.height * scale } }

        SetSmallestSize f ->
            { model | viewPos = ViewPos model.viewPos.c f }

        ZoomPoint f ( x, y ) ->
            let
                vp =
                    model.viewPos

                factor =
                    clamp 0.01 2 <| 1 + f / 1000

                p =
                    Vec.sub (mapIn (vec2 x y) model) vp.c

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


svgAttributes : Model -> List (Svg.Attribute Msg)
svgAttributes model =
    [ computeViewBox model
    , Wheel.onWheel (\e -> ZoomPoint e.deltaY e.mouseEvent.offsetPos)
    , Html.Attributes.id model.id
    , Svg.attribute "width" "100%"
    , Svg.attribute "height" "100%"
    , SA.preserveAspectRatio TypedSvg.Types.AlignNone TypedSvg.Types.Meet
    ]


computeViewBox : Model -> Svg.Attribute Msg
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
