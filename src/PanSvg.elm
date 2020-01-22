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


init : Model
init =
    { svgSize = Size 0 0
    , viewPos = ViewPos (vec2 0 0) 10
    }


type Msg
    = SVGSize (Result D.Error Size)
    | Zoom Float ( Float, Float )
    | Pan Direction


type Direction
    = Left
    | Right
    | Up
    | Down


update : Msg -> Model -> Model
update msg model =
    case msg of
        SVGSize res ->
            case res of
                Result.Err e ->
                    Debug.log (D.errorToString e) model

                Result.Ok s ->
                    { model | svgSize = s }

        Zoom f ( x, y ) ->
            let
                vp =
                    model.viewPos

                factor =
                    1 + f / 1000

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


sub : Sub Msg
sub =
    newSVGSize (SVGSize << D.decodeValue sizeDecoder)


svgAttributes : Model -> List (Svg.Attribute Msg)
svgAttributes model =
    [ computeViewBox model
    , Wheel.onWheel (\e -> Zoom e.deltaY e.mouseEvent.offsetPos)
    , Html.Attributes.id "svg"
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
