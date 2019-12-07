module Wheel exposing (..)

import Coll exposing (Id)
import Color
import Content exposing (Content)
import Html.Attributes
import Interact
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Math.Vector2 exposing (..)
import Sound
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (Fill(..), Length(..), Opacity(..), Transform(..))


type alias Wheeled a =
    { a | wheel : Wheel }


type alias Wheel =
    { name : String
    , startPercent : Float
    , volume : Float
    , content : WheelContent
    , mute : Bool
    }


type WheelContent
    = C (Content Wheel)


getContent : Wheeled g -> Content Wheel
getContent { wheel } =
    case wheel.content of
        C c ->
            c


default : Wheel
default =
    { name = ""
    , startPercent = 0
    , volume = 1
    , content = C <| Content.S Sound.noSound
    , mute = False
    }


fromSound : Sound.Sound -> Wheel
fromSound s =
    { default | content = C <| Content.S s }


type Mod
    = None
    | Selectable
    | Selected
    | Resizing


type alias Style =
    { mod : Mod, motor : Bool, dashed : Bool }


type Interactable x
    = IWheel (Id x)
    | IResizeHandle (Id x) Bool -- True = Right


type Msg
    = ChangeContent (Content Wheel)
    | ChangeVolume Float
    | Named String


update : Msg -> Wheeled g -> Wheeled g
update msg g =
    let
        wheel =
            g.wheel
    in
    case msg of
        ChangeContent c ->
            { g | wheel = { wheel | content = C c } }

        ChangeVolume vol ->
            { g | wheel = { wheel | volume = vol } }

        Named name ->
            if String.all (\c -> Char.isAlphaNum c || c == '-') name then
                { g | wheel = { wheel | name = name } }

            else
                g


view : Wheel -> Vec2 -> Float -> Style -> Id x -> String -> Svg (Interact.Msg (Interactable x))
view w pos length style id uid =
    let
        tickH =
            length / 15

        tickW =
            length / 30

        circum =
            length * pi
    in
    S.g
        ([ SA.transform [ Translate (getX pos) (getY pos) ] ]
            ++ Interact.hoverEvents (IWheel id)
        )
        ([ S.g [ Html.Attributes.id uid ]
            [ S.circle
                ([ SA.cx <| Num 0
                 , SA.cy <| Num 0
                 , SA.r <| Num (length / 2)
                 , SA.stroke <|
                    if style.motor then
                        Color.red

                    else
                        Color.black
                 , SA.strokeWidth <|
                    Num <|
                        if style.mod == Selectable then
                            tickW * 2

                        else
                            tickW
                 , SA.strokeDasharray <|
                    if style.dashed then
                        String.fromFloat (circum / 40 * 3 / 4)
                            ++ ","
                            ++ String.fromFloat (circum / 40 * 1 / 4)

                    else
                        ""
                 , SA.fill <|
                    if w.mute then
                        Fill Color.white

                    else
                        Fill Color.black
                 , SA.fillOpacity <| Opacity (0.2 + 0.8 * w.volume)
                 ]
                    ++ Interact.draggableEvents (IWheel id)
                )
                []
            , S.rect
                [ SA.width <| Num tickW
                , SA.height <| Num tickH
                , SA.x <| Num (tickW / -2)
                , SA.y <| Num ((length / -2) - tickH)
                ]
                []
            , S.rect
                [ SA.width <| Num tickW
                , SA.height <| Num tickH
                , SA.x <| Num (tickW / -2)
                , SA.y <| Num (tickH / -2)
                , SA.fill <| Fill Color.orange
                , SA.transform [ Rotate (w.startPercent * 360) 0 0, Translate 0 ((length / -2) + (tickH / 2)) ]
                ]
                []
            ]
         ]
            ++ (if style.mod == Selected then
                    [ S.circle
                        [ SA.cx <| Num 0
                        , SA.cy <| Num 0
                        , SA.r <| Num (length / 2 + tickW * 2)
                        , SA.strokeWidth <| Num (tickW / 2)
                        , SA.stroke Color.black
                        , SA.fill FillNone
                        ]
                        []
                    ]

                else
                    []
               )
            ++ (if style.mod == Resizing then
                    [ S.polyline
                        [ SA.points [ ( -length / 2, 0 ), ( length / 2, 0 ) ]
                        , SA.stroke Color.red
                        , SA.strokeWidth <| Num tickW
                        ]
                        []
                    , S.circle
                        ([ SA.cx <| Num (-length / 2)
                         , SA.cy <| Num 0
                         , SA.r <| Num (tickW * 2)
                         ]
                            ++ Interact.draggableEvents (IResizeHandle id False)
                        )
                        []
                    , S.circle
                        ([ SA.cx <| Num (length / 2)
                         , SA.cy <| Num 0
                         , SA.r <| Num (tickW * 2)
                         ]
                            ++ Interact.draggableEvents (IResizeHandle id True)
                        )
                        []
                    ]

                else
                    []
               )
        )


encoder : Wheel -> List ( String, E.Value )
encoder w =
    [ ( "name", E.string w.name )
    , ( "startPercent", E.float w.startPercent )
    , ( "volume", E.float w.volume )
    , ( "mute", E.bool w.mute )
    , case w.content of
        C (Content.S s) ->
            ( "sound", Sound.encoder s )

        _ ->
            Debug.todo "Encode content"
    ]


decoder : D.Decoder Wheel
decoder =
    Field.attempt "name" D.string <|
        \name ->
            Field.require "startPercent" D.float <|
                \startPercent ->
                    Field.require "volume" D.float <|
                        \volume ->
                            Field.require "sound" Sound.decoder <|
                                \sound ->
                                    Field.require "mute" D.bool <|
                                        \mute ->
                                            D.succeed
                                                { name = Maybe.withDefault "" name
                                                , startPercent = startPercent
                                                , volume = volume
                                                , content = C <| Content.S sound
                                                , mute = mute
                                                }
