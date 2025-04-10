module Data.Wheel exposing (Wheel, default, fromSoundAndInterval, getEngined, getPos, pupilID, view)

import Color exposing (Color)
import Data.Content as Content exposing (Bead, Content, Mobile)
import Data.Pupil as Pupil exposing (Pupil)
import Html.Attributes
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Math.Vector2 exposing (..)
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Length(..), Opacity(..), Transform(..))
import Utils.Utils exposing (WheelStyle, drawWheel, unmaybeMap)


type Wheel
    = Model Internals


pupilID : String
pupilID =
    "pupil"



-- TODO exposing Model ? pourquoi le cacher ?
--
-- TODO put pos there, but what if this is a bead ?
-- TODO same problem with pack…


type alias Internals =
    { name : String
    , pos : Vec2
    , interval : Float -- millis
    , launchPercent : Float
    , pupil : Maybe Pupil
    }


type alias Engined =
    { name : String
    , interval : Float
    , launchPercent : Float
    , pupil : Maybe Pupil
    }


type alias Cosmetics =
    { pos : Vec2
    , pupilHue : Float
    }


type alias Conteet =
    Content Wheel


fromSoundAndInterval : Sound -> Float -> Float -> Cosmetics -> Wheel
fromSoundAndInterval sound dur start { pos, pupilHue } =
    Model
        { name = Sound.getName sound
        , pos = pos
        , interval = dur
        , launchPercent = start
        , pupil = Just <| Pupil.fromSound sound pupilHue
        }


getEngined : Wheel -> Engined
getEngined (Model model) =
    { name = model.name
    , interval = model.interval
    , launchPercent = model.launchPercent
    , pupil = model.pupil
    }


getContent : Wheel -> Maybe Conteet
getContent (Model model) =
    Maybe.map Pupil.getContent model.pupil


getPos : Wheel -> Vec2
getPos (Model w) =
    w.pos


setPupil : Maybe Pupil -> Wheel -> Wheel
setPupil pupil (Model model) =
    Model { model | pupil = pupil }


{-| Needed for Coll…
-}
default : Wheel
default =
    Model
        { name = ""
        , pos = vec2 0 0
        , interval = -1
        , launchPercent = 0
        , pupil = Nothing
        }



--fromContent : Conteet -> Wheel
--fromContent c =
--    (update (ChangeStart 0) { wheel = { default | content = C c } }).wheel


type Msg
    = ChangeContent Conteet
    | ChangeVolume Float
    | ToggleMute
    | Mute Bool
    | ChangeStart Float
    | ChangeLoop ( Maybe Float, Maybe Float )
    | ChangeDiv Int Float
    | Named String
    | ChangeColor Float
    | ToggleContentView



--update : Msg -> Wheel -> Wheel
--update msg g =
--    let
--        wheel =
--            g.wheel
--
--        --TODO Very specific to beads, but content or collar doesn’t know wheels, so where is it to put ?
--        chgLoopWithSoundLength : ( Maybe Float, Maybe Float ) -> Bead Wheel -> Bead Wheel
--        chgLoopWithSoundLength p b =
--            let
--                w =
--                    b.wheel
--
--                newSound =
--                    case getWheelContent w of
--                        Content.S s ->
--                            Sound.setLoop p s
--
--                        _ ->
--                            Sound.noSound
--
--                newWheel =
--                    { w
--                        | startPercent = Tuple.first <| Sound.getLoopPercents newSound
--                        , content = C <| Content.S newSound
--                    }
--
--                length =
--                    Sound.length newSound
--            in
--            { wheel = newWheel, length = length }
--    in
--    case msg of
--        ChangeContent c ->
--            { g | wheel = { wheel | content = C c } }
--
--        ChangeVolume vol ->
--            { g | wheel = { wheel | volume = clamp 0 1 vol } }
--
--        ToggleMute ->
--            { g | wheel = { wheel | mute = not wheel.mute } }
--
--        Mute b ->
--            { g | wheel = { wheel | mute = b } }
--
--        ChangeStart percent ->
--            let
--                ( min, max ) =
--                    case wheel.content of
--                        C (Content.S s) ->
--                            Sound.getLoopPercents s
--
--                        _ ->
--                            ( 0, 1 )
--            in
--            { g | wheel = { wheel | startPercent = clamp min max percent } }
--
--        ChangeLoop mayPoints ->
--            case wheel.content of
--                C (Content.S s) ->
--                    let
--                        newSound =
--                            Sound.setLoop mayPoints s
--
--                        ( min, max ) =
--                            Sound.getLoopPercents newSound
--                    in
--                    { g
--                        | wheel =
--                            { wheel
--                                | content = C <| Content.S newSound
--                                , startPercent = clamp min max wheel.startPercent
--                            }
--                    }
--
--                C (Content.C c) ->
--                    { g
--                        | wheel =
--                            { wheel
--                                | content =
--                                    C <|
--                                        Content.C <|
--                                            Content.setCollarLoop chgLoopWithSoundLength mayPoints c
--                            }
--                    }
--
--                _ ->
--                    g
--
--        ChangeDiv i percent ->
--            case wheel.content of
--                C (Content.C c) ->
--                    { g
--                        | wheel =
--                            { wheel
--                                | content = C <| Content.C <| Content.setCollarDiv chgLoopWithSoundLength i percent c
--                            }
--                    }
--
--                _ ->
--                    g
--
--        Named name ->
--            if String.all (\c -> Char.isAlphaNum c || c == '-') name then
--                { g | wheel = { wheel | name = name } }
--
--            else
--                g
--
--        ChangeColor c ->
--            { g | wheel = { wheel | color = c } }
--
--        ToggleContentView ->
--            { g | wheel = { wheel | viewContent = not wheel.viewContent } }


view :
    Wheel
    -> WheelStyle
    -> List (Attribute msg)
    -> String
    -> Maybe (Svg msg)
    -> Svg msg
view (Model model) style attrs uid maySymbol =
    let
        stroke =
            model.interval / 30

        axis =
            S.circle
                [ SA.cx <| Num 0
                , SA.cy <| Num 0
                , SA.r <| Num stroke
                , SA.strokeWidth <| Num stroke
                ]
                []

        pupil =
            case model.pupil of
                Nothing ->
                    []

                Just p ->
                    let
                        pupilLength =
                            Pupil.getLength p

                        pupilAngle =
                            model.launchPercent * 2 * pi - pi / 2

                        pupilX =
                            pupilLength / 2 * cos pupilAngle

                        pupilY =
                            pupilLength / 2 * sin pupilAngle

                        pupilTranslate =
                            SA.transform [ Translate pupilX pupilY ]
                    in
                    [ S.g
                        [ pupilTranslate

                        --, Html.Attributes.id <| uid ++ "-animate-pupil"
                        --, SA.transform [ Rotate 0 -pupilX -pupilY ]
                        ]
                        [ Pupil.view p
                            (uid ++ pupilID)
                            [ SA.opacity <| Opacity 0.5
                            , Html.Attributes.attribute "rx" <| String.fromFloat -pupilX
                            , Html.Attributes.attribute "ry" <| String.fromFloat -pupilY
                            ]
                        ]
                    ]

        interval =
            S.g
                [ SA.transform [ Translate 0 -(model.interval / 2) ] ]
                [ drawWheel
                    model.interval
                    Nothing
                    style
                    uid
                    [ Html.Attributes.attribute "rx" "0"
                    , Html.Attributes.attribute "ry" <| String.fromFloat (model.interval / 2)
                    ]
                    []
                    [ S.rect
                        [ SA.width <| Num <| stroke / 2
                        , SA.height <| Num model.interval
                        , SA.y <| Num <| -model.interval / 2
                        ]
                        []
                    ]
                ]

        toolView =
            unmaybeMap maySymbol
    in
    S.g
        (SA.transform [ Translate (getX model.pos) (getY model.pos) ]
            :: attrs
        )
        (pupil ++ [ interval, axis ])



--    S.g
--    (SA.transform [ Translate (getX pos) (getY pos) ]
--        :: (if style.weaving then
--                [ SA.opacity <| Opacity 0.5 ]
--
--            else
--                []
--           )
--    )
--<|
--    (case style.named of
--        Just name ->
--            [ S.text_
--                [ SA.x <| Num 0
--                , SA.y <| Num -(d * 3 / 4)
--                , SA.fontSize <| Num (d / 2)
--                , SA.textAnchor AnchorMiddle
--                , SA.stroke Color.white
--                , SA.strokeWidth <| Num (tickW / 4)
--                ]
--                [ text name ]
--            ]
--
--        Nothing ->
--            [ S.text_ [] [] ]
--     -- Because rotating g cannot be Keyed in TypedSvg, trick to prevent recreation
--    )
--        ++ [ S.g hoverAttrs <|
--                ([ S.g
--                    -- rotation and drag
--                    (if String.isEmpty uid then
--                        []
--
--                     else
--                        [ Html.Attributes.id uid ] ++ dragAttrs
--                    )
--                    ([ S.circle
--                        [ SA.cx <| Num 0
--                        , SA.cy <| Num 0
--                        , SA.r <| Num (d / 2)
--                        , SA.stroke <|
--                            if style.motor then
--                                Color.red
--
--                            else
--                                Color.black
--                        , SA.strokeWidth <|
--                            Num <|
--                                if style.mod == Selectable then
--                                    tickW * 2
--
--                                else
--                                    tickW
--                        , SA.strokeDasharray <|
--                            if style.dashed then
--                                String.fromFloat (circum / 40 * 3 / 4)
--                                    ++ ","
--                                    ++ String.fromFloat (circum / 40 * 1 / 4)
--
--                            else
--                                ""
--                        , SA.fill <| FillNone
--                        ]
--                        []
--                     , S.rect
--                        [ SA.width <| Num tickW
--                        , SA.height <| Num tickH
--                        , SA.x <| Num (tickW / -2)
--                        , SA.y <| Num (tickH / -2)
--                        , SA.transform [ Rotate (startPercent * 360) 0 0, Translate 0 ((d / -2) - (tickH / 2)) ]
--                        ]
--                        []
--                     ]
--                        ++ (case style.baseColor of
--                                Just c ->
--                                    [ S.circle
--                                        [ SA.cx <| Num 0
--                                        , SA.cy <| Num 0
--                                        , SA.r <| Num (d / 2 - tickW * 2.5)
--                                        , SA.strokeWidth <| Num (tickW * 4)
--                                        , SA.stroke <| Color.hsl c 1 0.5
--                                        , SA.fill FillNone
--                                        ]
--                                        []
--                                    ]
--
--                                Nothing ->
--                                    []
--                           )
--                    )
--
--                 -- end rotation drag
--                 ]
--                    -- No drag events part
--                    ++ (case style.mod of
--                            Selected first ->
--                                [ S.circle
--                                    [ SA.cx <| Num 0
--                                    , SA.cy <| Num 0
--                                    , SA.r <| Num (d / 2 + tickW * 2)
--                                    , SA.strokeWidth <| Num (tickW / 2)
--                                    , SA.stroke <|
--                                        if first then
--                                            Color.red
--
--                                        else
--                                            Color.black
--                                    , SA.fill FillNone
--                                    , SA.opacity <| Opacity 0.5
--                                    ]
--                                    []
--                                ]
--
--                            Resizing ->
--                                --case mayHandleInteract of
--                                --    Just handle ->
--                                --        [ S.polyline
--                                --            [ SA.points [ ( -d / 2, 0 ), ( d / 2, 0 ) ]
--                                --            , SA.stroke Color.red
--                                --            , SA.strokeWidth <| Num tickW
--                                --            ]
--                                --            []
--                                --        , S.circle
--                                --            ([ SA.cx <| Num (-d / 2)
--                                --             , SA.cy <| Num 0
--                                --             , SA.r <| Num (tickW * 2)
--                                --             ]
--                                --                ++ Interact.draggableEvents (handle False)
--                                --            )
--                                --            []
--                                --        , S.circle
--                                --            ([ SA.cx <| Num (d / 2)
--                                --             , SA.cy <| Num 0
--                                --             , SA.r <| Num (tickW * 2)
--                                --             ]
--                                --                ++ Interact.draggableEvents (handle True)
--                                --            )
--                                --            []
--                                --        ]
--                                --
--                                --    Nothing ->
--                                []
--
--                            _ ->
--                                []
--                       )
--                )
--           ]
-- end hover
--insideCollarView :
--    Content.Collar Wheel
--    -> Maybe ( List Int -> interact, List Int )
--    -> String
--    -> List (Svg (Interact.Msg interact x))
--insideCollarView collar mayWheelInter parentUid =
--    Tuple.first <|
--        List.foldl
--            (\b ( res, ( p, i ) ) ->
--                ( view b.wheel
--                    (vec2 (p + b.length / 2) 0)
--                    b.length
--                    { defaultStyle | named = Nothing }
--                    (Maybe.map (\( interact, l ) -> ( interact, l ++ [ i ] )) mayWheelInter)
--                    Nothing
--                    (Content.beadUIDExtension parentUid i)
--                    :: res
--                , ( p + b.length
--                  , i + 1
--                  )
--                )
--            )
--            ( [], ( 0, 0 ) )
--            (Content.getBeads collar)
--encoder : Wheel -> List ( String, E.Value )
--encoder w =
--    [ ( "name", E.string w.name )
--    , ( "startPercent", E.float w.startPercent )
--    , ( "volume", E.float w.volume )
--    , ( "mute", E.bool w.mute )
--    , ( "color", E.float w.color )
--    , ( "viewContent", E.bool w.viewContent )
--    , case w.content of
--        C c ->
--            Content.encoder encoder c
--    ]
--
--
--decoder : (Conteet -> Float) -> D.Decoder Wheel
--decoder getContentLength =
--    Content.decoder (D.lazy (\_ -> decoder getContentLength)) (getContentLength << getWheelContent) default
--        |> D.andThen
--            (\content ->
--                Field.attempt "viewContent" D.bool <|
--                    \viewContent ->
--                        Field.attempt "name" D.string <|
--                            \name ->
--                                Field.require "startPercent" D.float <|
--                                    \startPercent ->
--                                        Field.require "volume" D.float <|
--                                            \volume ->
--                                                Field.require "mute" D.bool <|
--                                                    \mute ->
--                                                        Field.attempt "color" D.float <|
--                                                            \mayColor ->
--                                                                Field.attemptAt [ "color", "hue" ] D.float <|
--                                                                    \mayHue ->
--                                                                        D.succeed
--                                                                            { name = Maybe.withDefault "" name
--                                                                            , startPercent = startPercent
--                                                                            , volume = volume
--                                                                            , content = C content
--                                                                            , viewContent = Maybe.withDefault True viewContent
--                                                                            , mute = mute
--                                                                            , color =
--                                                                                case mayColor of
--                                                                                    Just c ->
--                                                                                        c
--
--                                                                                    Nothing ->
--                                                                                        case mayHue of
--                                                                                            Just h ->
--                                                                                                h
--
--                                                                                            Nothing ->
--                                                                                                0
--                                                                            }
--            )
