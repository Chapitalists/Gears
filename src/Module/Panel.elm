module Module.Panel exposing (..)

import Browser
import Color
import Element exposing (..)
import Element.Background as Bg
import Html exposing (Html)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { side : Side
    , size : Int
    }


type Side
    = Left
    | Right
    | Top
    | Bottom


init : Model
init =
    { side = Left
    , size = 200
    }


type Msg
    = UpdateSize Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSize size ->
            { model | size = size }


view : Model -> Html Msg
view { size } =
    layout [] <|
        el
            [ height fill
            , width <| minimum size <| maximum size <| fill
            , Bg.color <| toElColor <| Color.charcoal
            ]
            none


toElColor : Color.Color -> Color
toElColor c =
    let
        { red, green, blue, alpha } =
            Color.toRgba c
    in
    rgba red green blue alpha
