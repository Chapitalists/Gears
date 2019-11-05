module Wheel exposing (..)

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


subs _ =
    Sub.none


type alias Wheel =
    { size : Int
    , position : ( Int, Int )
    , toggle : Bool
    , color : Color
    , startPos : Float
    }


type Color
    = AnyColor


init () =
    ( Wheel 100 ( 150, 150 ) False AnyColor 0, Cmd.none )


type Msg
    = Clicked
    | Noop


update : Msg -> Wheel -> ( Wheel, Cmd Msg )
update msg model =
    case msg of
        Clicked ->
            ( { model | toggle = not model.toggle }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


view : Wheel -> Html Msg
view w =
    Html.div []
        [ Html.text <|
            if w.toggle then
                "OUI"

            else
                "NON"
        , svg [ viewBox "0 0 1000 1000" ]
            [ symbol [ viewBox "0 0 10 12", id "gearSym" ]
                [ circle
                    [ cx "5", cy "7", r "5" ]
                    []
                , rect [ fill "red", width "1", height "2", x "4.5", y "0" ] []
                , rect [ fill "purple", width "1", height "2", x "4.5", y "2" ] []
                ]
            , use [ xlinkHref "#gearSym", x "10", y "10", width "50", height "50", onClick Clicked ] []
            , use [ onClick Noop, xlinkHref "#gearSym", x "50", y "50", width "50", height "50" ] []
            ]
        ]
