port module Waveform exposing (..)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Html exposing (canvas)
import Html.Attributes as Attr
import Json.Decode as D
import Sound exposing (Sound)



-- TODO Maybe better to compute min max rms in js but draw in elm with adequate lib


port requestSoundDraw : String -> Cmd msg


port soundDrawn : (D.Value -> msg) -> Sub msg


canvasId : String
canvasId =
    "waveform"


type alias Waveform =
    { size : Int
    , drawn : Drawing
    }


type Drawing
    = None
    | SoundDrawn Sound
    | Pending Sound


init : Waveform
init =
    { size = 1000
    , drawn = None
    }


type Msg
    = GotSize Int
    | ChgSound Sound
    | GotDrawn (Result D.Error String)


update : Msg -> Waveform -> ( Waveform, Cmd Msg )
update msg wave =
    case msg of
        GotSize size ->
            ( { wave | size = size }
            , case wave.drawn of
                SoundDrawn s ->
                    requestSoundDraw <| Sound.toString s

                _ ->
                    Cmd.none
            )

        ChgSound s ->
            if wave.drawn == SoundDrawn s then
                ( wave, Cmd.none )

            else
                ( { wave | drawn = Pending s }, requestSoundDraw <| Sound.toString s )

        GotDrawn res ->
            case res of
                Ok str ->
                    case wave.drawn of
                        Pending s ->
                            if Sound.toString s == str then
                                ( { wave | drawn = SoundDrawn s }, Cmd.none )

                            else
                                ( wave, requestSoundDraw <| Sound.toString s )

                        _ ->
                            ( wave, Cmd.none )

                Err err ->
                    Debug.log ("Error while drawing " ++ D.errorToString err) ( wave, Cmd.none )


sub : Sub Msg
sub =
    soundDrawn (GotDrawn << D.decodeValue D.string)


view : Bool -> Waveform -> Float -> Element msg
view visible { size } percent =
    let
        border =
            2
    in
    el
        [ htmlAttribute <| Attr.hidden <| not visible
        , Border.color <| rgb 0 0 0
        , Border.width border
        , Bg.color <| rgb 1 1 1
        , alignBottom
        ]
    <|
        html <|
            canvas
                [ Attr.hidden <| not visible
                , Attr.id canvasId
                , Attr.width (size - 2 * border)
                ]
                []
