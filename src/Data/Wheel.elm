module Data.Wheel exposing (..)

import Color exposing (Color)
import Data.Content as Content exposing (Content, Mobile)
import FlipBook exposing (FlipBook)
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
    , startPercent : Float -- Percent of whole sound, not just looped part
    , volume : Float
    , content : WheelContent
    , viewContent : Bool
    , mute : Bool
    , color : Float
    , flip : FlipBook
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


getLoopPercents : Wheeled g -> ( Float, Float )
getLoopPercents { wheel } =
    case wheel.content of
        C (Content.S s) ->
            Sound.getLoopPercents s

        _ ->
            ( 0, 1 )


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
    , color = 0
    , flip = FlipBook.init []
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
    , baseColor : Maybe Float
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
    | Mute Bool
    | ChangeStart Float
    | ChangeLoop ( Maybe Float, Maybe Float )
    | Named String
    | ChangeColor Float
    | FlipMsg FlipBook.Msg
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

        Mute b ->
            { g | wheel = { wheel | mute = b } }

        ChangeStart percent ->
            let
                ( min, max ) =
                    case wheel.content of
                        C (Content.S s) ->
                            Sound.getLoopPercents s

                        _ ->
                            ( 0, 1 )
            in
            { g | wheel = { wheel | startPercent = clamp min max percent } }

        ChangeLoop mayPoints ->
            case wheel.content of
                C (Content.S s) ->
                    let
                        newSound =
                            Sound.setLoop mayPoints s

                        ( min, max ) =
                            Sound.getLoopPercents newSound
                    in
                    { g
                        | wheel =
                            { wheel
                                | content = C <| Content.S newSound
                                , startPercent = clamp min max wheel.startPercent
                            }
                    }

                _ ->
                    g

        Named name ->
            if String.all (\c -> Char.isAlphaNum c || c == '-') name then
                { g | wheel = { wheel | name = name } }

            else
                g

        ChangeColor c ->
            { g | wheel = { wheel | color = c } }

        FlipMsg subMsg ->
            { g | wheel = { wheel | flip = FlipBook.update subMsg wheel.flip } }

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

        ( loopStart, loopEnd ) =
            getLoopPercents { wheel = w }

        tickPercent =
            (w.startPercent - loopStart) / (loopEnd - loopStart)

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
                [ text <|
                    if String.isEmpty w.name then
                        case getWheelContent w of
                            Content.S s ->
                                Sound.fileName s

                            _ ->
                                ""

                    else
                        w.name
                ]
            ]

         else
            [ S.text_ [] [] ]
         -- Because rotating g cannot be Keyed in TypedSvg, trick to prevent recreation
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
                             ]
                                ++ (if w.mute then
                                        [ SA.fill <| Fill Color.white ]

                                    else
                                        [ SA.fill <| Fill <| Color.hsl w.color 1 (0.85 - 0.35 * w.volume) ]
                                   )
                            )
                            []
                         , S.rect
                            [ SA.width <| Num tickW
                            , SA.height <| Num tickH
                            , SA.x <| Num (tickW / -2)
                            , SA.y <| Num (tickH / -2)
                            , SA.transform [ Rotate (tickPercent * 360) 0 0, Translate 0 ((length / -2) - (tickH / 2)) ]
                            ]
                            []
                         ]
                            ++ (if w.mute then
                                    []

                                else
                                    [ FlipBook.view w.flip length ]
                               )
                            ++ (case style.baseColor of
                                    Just c ->
                                        [ S.circle
                                            [ SA.cx <| Num 0
                                            , SA.cy <| Num 0
                                            , SA.r <| Num (length / 2 - tickW * 2.5)
                                            , SA.strokeWidth <| Num (tickW * 4)
                                            , SA.stroke <| Color.hsl c 1 0.5
                                            , SA.fill FillNone
                                            ]
                                            []
                                        ]

                                    Nothing ->
                                        []
                               )
                        )

                     -- end rotation drag
                     ]
                        -- No drag events part
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
                    { defaultStyle | named = False }
                    (Maybe.map (\( inter, l ) -> ( inter, l ++ [ i ] )) mayWheelInter)
                    Nothing
                    (Content.beadUIDExtension parentUid i)
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
    , ( "color", E.float w.color )
    , ( "images", E.list E.string w.flip.urls )
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
                                                        Field.attempt "color" D.float <|
                                                            \mayColor ->
                                                                Field.attemptAt [ "color", "hue" ] D.float <|
                                                                    \mayHue ->
                                                                        Field.attempt "images" (D.list D.string) <|
                                                                            \mayImages ->
                                                                                D.succeed
                                                                                    { name = Maybe.withDefault "" name
                                                                                    , startPercent = startPercent
                                                                                    , volume = volume
                                                                                    , content = C content
                                                                                    , viewContent = Maybe.withDefault True viewContent
                                                                                    , mute = mute
                                                                                    , color =
                                                                                        case mayColor of
                                                                                            Just c ->
                                                                                                c

                                                                                            Nothing ->
                                                                                                case mayHue of
                                                                                                    Just h ->
                                                                                                        h

                                                                                                    Nothing ->
                                                                                                        0
                                                                                    , flip =
                                                                                        FlipBook.init <|
                                                                                            case mayImages of
                                                                                                Just l ->
                                                                                                    l

                                                                                                Nothing ->
                                                                                                    []
                                                                                    }
            )
