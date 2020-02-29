module Data.Wheel exposing (..)

import Color exposing (Color)
import Data.Content as Content exposing (Content, Mobile)
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
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Length(..), Opacity(..), Transform(..))


type alias Wheeled a =
    { a | wheel : Wheel }


type alias Wheel =
    { name : String
    , startPercent : Float
    , volume : Float
    , content : WheelContent
    , viewContent : Bool
    , mute : Bool
    , color : Color
    }


type alias Conteet =
    Content Wheel


type WheelContent
    = C Conteet


getContent : Wheeled g -> Conteet
getContent { wheel } =
    case wheel.content of
        C c ->
            c


getWheelContent : Wheel -> Conteet
getWheelContent { content } =
    case content of
        C c ->
            c


setContent : Conteet -> Wheeled g -> Wheeled g
setContent c g =
    let
        w =
            g.wheel
    in
    { g | wheel = { w | content = C c } }


default : Wheel
default =
    { name = ""
    , startPercent = 0
    , volume = 1
    , content = C <| Content.S Sound.noSound
    , viewContent = True
    , mute = False
    , color = Color.black
    }


fromContent : Conteet -> Wheel
fromContent c =
    { default | content = C c }


type Mod
    = None
    | Selectable
    | Selected Bool
    | Resizing


type alias Style =
    { mod : Mod
    , motor : Bool
    , dashed : Bool
    , baseColor : Maybe Color
    , named : Bool
    }


defaultStyle : Style
defaultStyle =
    { mod = None
    , motor = False
    , dashed = False
    , baseColor = Nothing
    , named = True
    }


type Msg
    = ChangeContent Conteet
    | ChangeVolume Float
    | ToggleMute
    | ChangeStart Float
    | Named String
    | ChangeColor Color
    | ToggleContentView


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
            { g | wheel = { wheel | volume = clamp 0 1 vol } }

        ToggleMute ->
            { g | wheel = { wheel | mute = not wheel.mute } }

        ChangeStart percent ->
            { g | wheel = { wheel | startPercent = percent } }

        Named name ->
            if String.all (\c -> Char.isAlphaNum c || c == '-') name then
                { g | wheel = { wheel | name = name } }

            else
                g

        ChangeColor c ->
            { g | wheel = { wheel | color = c } }

        ToggleContentView ->
            { g | wheel = { wheel | viewContent = not wheel.viewContent } }


view :
    Wheel
    -> Vec2
    -> Float
    -> Style
    -> Maybe ( List Int -> inter, List Int )
    -> Maybe (Bool -> inter)
    -> String
    -> Svg (Interact.Msg inter x)
view w pos lengthTmp style mayWheelInter mayHandleInter uid =
    let
        ( viewContent, bigger ) =
            case w.content of
                C (Content.C col) ->
                    ( w.viewContent, w.viewContent && List.length col.beads == 0 )

                _ ->
                    ( False, False )

        length =
            if bigger then
                lengthTmp * 1.2

            else
                lengthTmp

        tickH =
            length / 15

        tickW =
            length / 30

        circum =
            length * pi

        ( hoverAttrs, dragAttrs ) =
            Maybe.withDefault ( [], [] ) <|
                Maybe.map
                    (\( inter, l ) -> ( Interact.hoverEvents <| inter l, Interact.draggableEvents <| inter l ))
                    mayWheelInter
    in
    S.g [ SA.transform [ Translate (getX pos) (getY pos) ] ] <|
        (if style.named then
            [ S.text_
                [ SA.x <| Num 0
                , SA.y <| Num -(length * 3 / 4)
                , SA.fontSize <| Num (length / 2)
                , SA.textAnchor AnchorMiddle
                , SA.stroke Color.white
                , SA.strokeWidth <| Num (tickW / 4)
                ]
                [ text w.name ]
            ]

         else
            []
        )
            ++ [ S.g hoverAttrs <|
                    ([ S.g
                        -- rotation and drag
                        (if String.isEmpty uid then
                            []

                         else
                            [ Html.Attributes.id uid ] ++ dragAttrs
                        )
                        ([ S.circle
                            [ SA.cx <| Num 0
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
                                    Fill w.color
                            , SA.fillOpacity <| Opacity (0.2 + 0.8 * w.volume)
                            ]
                            []
                         , S.rect
                            [ SA.width <| Num tickW
                            , SA.height <| Num tickH
                            , SA.x <| Num (tickW / -2)
                            , SA.y <| Num (tickH / -2)
                            , SA.transform [ Rotate (w.startPercent * 360) 0 0, Translate 0 ((length / -2) - (tickH / 2)) ]
                            ]
                            []
                         ]
                            ++ (case style.baseColor of
                                    Just c ->
                                        [ S.circle
                                            [ SA.cx <| Num 0
                                            , SA.cy <| Num 0
                                            , SA.r <| Num (length / 2 - tickW * 2.5)
                                            , SA.strokeWidth <| Num (tickW * 4)
                                            , SA.stroke c
                                            , SA.fill FillNone
                                            ]
                                            []
                                        ]

                                    Nothing ->
                                        []
                               )
                            ++ (if viewContent then
                                    case w.content of
                                        C (Content.C collar) ->
                                            let
                                                scale =
                                                    lengthTmp / Content.getMatriceLength collar
                                            in
                                            [ S.g [ SA.transform [ Translate (-lengthTmp / 2) 0, Scale scale scale ] ] <|
                                                insideCollarView collar mayWheelInter uid
                                            ]

                                        _ ->
                                            Debug.todo "view Sound or Mobile inside wheel"

                                else
                                    let
                                        symSize =
                                            length / 4
                                    in
                                    case w.content of
                                        C (Content.M _) ->
                                            [ S.line
                                                [ SA.x1 <| Num -symSize
                                                , SA.y1 <| Num -symSize
                                                , SA.x2 <| Num symSize
                                                , SA.y2 <| Num symSize
                                                , SA.stroke Color.grey
                                                , SA.strokeWidth <| Num tickW
                                                ]
                                                []
                                            , S.line
                                                [ SA.x1 <| Num -symSize
                                                , SA.y1 <| Num symSize
                                                , SA.x2 <| Num symSize
                                                , SA.y2 <| Num -symSize
                                                , SA.stroke Color.grey
                                                , SA.strokeWidth <| Num tickW
                                                ]
                                                []
                                            ]

                                        C (Content.C _) ->
                                            [ S.line
                                                [ SA.x1 <| Num -symSize
                                                , SA.y1 <| Num 0
                                                , SA.x2 <| Num symSize
                                                , SA.y2 <| Num 0
                                                , SA.stroke Color.grey
                                                , SA.strokeWidth <| Num tickW
                                                ]
                                                []
                                            ]

                                        _ ->
                                            []
                               )
                        )

                     -- end rotation drag
                     ]
                        -- No drag events part
                        ++ (case style.mod of
                                Selected first ->
                                    [ S.circle
                                        [ SA.cx <| Num 0
                                        , SA.cy <| Num 0
                                        , SA.r <| Num (length / 2 + tickW * 2)
                                        , SA.strokeWidth <| Num (tickW / 2)
                                        , SA.stroke <|
                                            if first then
                                                Color.red

                                            else
                                                Color.black
                                        , SA.fill FillNone
                                        ]
                                        []
                                    ]

                                Resizing ->
                                    case mayHandleInter of
                                        Just handle ->
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
                                                    ++ Interact.draggableEvents (handle False)
                                                )
                                                []
                                            , S.circle
                                                ([ SA.cx <| Num (length / 2)
                                                 , SA.cy <| Num 0
                                                 , SA.r <| Num (tickW * 2)
                                                 ]
                                                    ++ Interact.draggableEvents (handle True)
                                                )
                                                []
                                            ]

                                        Nothing ->
                                            []

                                _ ->
                                    []
                           )
                    )
               ]



-- end hover


insideCollarView :
    Content.Collar Wheel
    -> Maybe ( List Int -> inter, List Int )
    -> String
    -> List (Svg (Interact.Msg inter x))
insideCollarView collar mayWheelInter parentUid =
    Tuple.first <|
        List.foldl
            (\b ( res, ( p, i ) ) ->
                ( view b.wheel
                    (vec2 (p + b.length / 2) 0)
                    b.length
                    defaultStyle
                    (Maybe.map (\( inter, l ) -> ( inter, l ++ [ i ] )) mayWheelInter)
                    Nothing
                    (parentUid ++ String.fromInt i)
                    :: res
                , ( p + b.length
                  , i + 1
                  )
                )
            )
            ( [], ( 0, 0 ) )
            (Content.getBeads collar)


encoder : Wheel -> List ( String, E.Value )
encoder w =
    [ ( "name", E.string w.name )
    , ( "startPercent", E.float w.startPercent )
    , ( "volume", E.float w.volume )
    , ( "mute", E.bool w.mute )
    , ( "color", colorEncoder w.color )
    , ( "viewContent", E.bool w.viewContent )
    , case w.content of
        C c ->
            Content.encoder encoder c
    ]


decoder : D.Decoder Wheel
decoder =
    Content.decoder (D.lazy (\_ -> decoder)) default
        |> D.andThen
            (\content ->
                Field.attempt "viewContent" D.bool <|
                    \viewContent ->
                        Field.attempt "name" D.string <|
                            \name ->
                                Field.require "startPercent" D.float <|
                                    \startPercent ->
                                        Field.require "volume" D.float <|
                                            \volume ->
                                                Field.require "mute" D.bool <|
                                                    \mute ->
                                                        Field.attempt "color" colorDecoder <|
                                                            \color ->
                                                                D.succeed
                                                                    { name = Maybe.withDefault "" name
                                                                    , startPercent = startPercent
                                                                    , volume = volume
                                                                    , content = C content
                                                                    , viewContent = Maybe.withDefault True viewContent
                                                                    , mute = mute
                                                                    , color = Maybe.withDefault Color.black color
                                                                    }
            )


colorEncoder : Color -> E.Value
colorEncoder c =
    let
        named =
            Color.toHsla c
    in
    E.object
        [ ( "hue", E.float named.hue )
        , ( "sat", E.float named.saturation )
        , ( "light", E.float named.lightness )
        , ( "alpha", E.float named.alpha )
        ]


colorDecoder : D.Decoder Color
colorDecoder =
    Field.require "hue" D.float <|
        \hue ->
            Field.require "sat" D.float <|
                \sat ->
                    Field.require "light" D.float <|
                        \light ->
                            Field.require "alpha" D.float <|
                                \alpha ->
                                    D.succeed <| Color.hsla hue sat light alpha
