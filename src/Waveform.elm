port module Waveform exposing (..)

import Editor.Interacting exposing (..)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Html exposing (canvas)
import Html.Attributes as Attr
import Html.Events.Extra.Wheel as Wheel
import Interact
import Json.Decode as D



-- TODO Maybe better to compute min max rms in js but draw in elm with adequate lib


port requestSoundDraw : SoundView -> Cmd msg


port soundDrawn : (D.Value -> msg) -> Sub msg


soundViewDecoder : D.Decoder SoundView
soundViewDecoder =
    D.map3 SoundView
        (D.field "soundName" D.string)
        (D.field "zoomFactor" D.float)
        (D.field "centerPercent" D.float)


canvasId : String
canvasId =
    "waveform"


border : Int
border =
    2


type alias Waveform =
    { size : Int
    , height : Int
    , drawn : Drawing
    , sel : Maybe ( Int, Int )
    }


type alias SoundView =
    { soundName : String
    , zoomFactor : Float
    , centerPercent : Float
    }


type Drawing
    = None
    | SoundDrawn SoundView
    | Pending SoundView


init : Waveform
init =
    { size = 1000
    , height = 150
    , drawn = None
    , sel = Nothing
    }


getSelPercents : Waveform -> Maybe ( Float, Float )
getSelPercents { sel, size } =
    let
        toPercent px =
            toFloat px / toFloat size
    in
    Maybe.map (Tuple.mapBoth toPercent toPercent) sel


isDrawn : Waveform -> String -> Bool
isDrawn { drawn } name =
    case drawn of
        SoundDrawn { soundName } ->
            name == soundName

        _ ->
            False


type Msg
    = GotSize Int
    | ChgSound String
    | ChgView Float Float -- Zoom, Center percent
    | ZoomPoint Float ( Float, Float )
    | GotDrawn (Result D.Error SoundView)
    | Select ( Float, Float )
    | MoveSel Float
    | CancelSel


update : Msg -> Waveform -> ( Waveform, Cmd Msg )
update msg wave =
    case msg of
        GotSize size ->
            ( { wave | size = size - 2 * border }
            , case wave.drawn of
                SoundDrawn sv ->
                    requestSoundDraw sv

                _ ->
                    Cmd.none
            )

        ChgSound name ->
            let
                newSV =
                    SoundView name 1 0.5

                chgRes =
                    ( { wave | drawn = Pending newSV }, requestSoundDraw newSV )
            in
            case wave.drawn of
                SoundDrawn { soundName } ->
                    if name == soundName then
                        ( wave, Cmd.none )

                    else
                        chgRes

                _ ->
                    chgRes

        ChgView factor center ->
            let
                f =
                    if factor < 1 then
                        1

                    else
                        factor

                c =
                    clamp (1 / f / 2) (1 - 1 / f / 2) center
            in
            case wave.drawn of
                SoundDrawn sv ->
                    if f /= sv.zoomFactor || c /= sv.centerPercent then
                        let
                            newSV =
                                { sv | zoomFactor = f, centerPercent = c }
                        in
                        ( { wave | drawn = Pending newSV }, requestSoundDraw newSV )

                    else
                        ( wave, Cmd.none )

                Pending { soundName } ->
                    let
                        newSV =
                            SoundView soundName f c
                    in
                    ( { wave | drawn = Pending newSV }, requestSoundDraw newSV )

                None ->
                    ( wave, Cmd.none )

        ZoomPoint delta ( x, y ) ->
            case wave.drawn of
                SoundDrawn sv ->
                    let
                        factor =
                            clamp 0.01 2 <| 1 + delta / 100

                        d =
                            x / toFloat wave.size - 0.5

                        f =
                            sv.zoomFactor * factor

                        c =
                            sv.centerPercent + d * (factor - 1) / f
                    in
                    update (ChgView f c) wave

                _ ->
                    ( wave, Cmd.none )

        GotDrawn res ->
            case res of
                Ok soundView ->
                    case wave.drawn of
                        Pending sv ->
                            if sv == soundView then
                                ( { wave | drawn = SoundDrawn sv }, Cmd.none )

                            else
                                ( wave, requestSoundDraw sv )

                        _ ->
                            ( wave, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log ("Error while drawing " ++ D.errorToString err) wave
                    in
                    ( wave, Cmd.none )

        Select ( centerPx, percentLength ) ->
            let
                halfSel =
                    round (percentLength * toFloat wave.size / 2)

                safeCenter =
                    clamp halfSel (wave.size - halfSel) <| round centerPx
            in
            ( { wave | sel = Just ( safeCenter - halfSel, safeCenter + halfSel ) }, Cmd.none )

        MoveSel d ->
            case wave.sel of
                Just ( px1, px2 ) ->
                    let
                        move =
                            (+) <| clamp -px1 (wave.size - px2) <| round d
                    in
                    ( { wave | sel = Just ( move px1, move px2 ) }, Cmd.none )

                Nothing ->
                    ( wave, Cmd.none )

        CancelSel ->
            ( { wave | sel = Nothing }, Cmd.none )


sub : Sub Msg
sub =
    soundDrawn (GotDrawn << D.decodeValue soundViewDecoder)


type Cursors
    = Sound { offset : Float, start : Float, end : Float }
    | CollarDiv { start : Float, end : Float, divs : List Float }


view :
    Waveform
    -> Maybe Cursors
    -> Interact.State Interactable Zone
    -> (Interact.Msg Interactable Zone -> msg)
    -> (Msg -> msg)
    -> Element msg
view wave mayCursors interState wrapInter wrapMsg =
    let
        toPx =
            round << ((*) <| toFloat wave.size)
    in
    el
        ((htmlAttribute <| Attr.map wrapMsg <| Wheel.onWheel (\e -> ZoomPoint -e.deltaY e.mouseEvent.offsetPos))
            :: (case mayCursors of
                    Just cursors ->
                        (List.map (htmlAttribute << Attr.map wrapInter) <| Interact.dragSpaceEvents interState ZWave)
                            ++ [ Border.color <| rgb 0 0 0
                               , Border.width border
                               , Bg.color <| rgb 1 1 1
                               , alignBottom
                               ]
                            ++ (List.map (mapAttribute wrapInter) <|
                                    case cursors of
                                        Sound c ->
                                            [ selection ( toPx 0, toPx c.start ) Nothing <| rgba 0.5 0.5 0.5 0.5
                                            , selection ( toPx c.end, toPx 1 ) Nothing <| rgba 0.5 0.5 0.5 0.5
                                            , selection ( toPx c.start, toPx c.end ) (Just IWaveSel) <| rgba 0 0 0 0
                                            , cursor (toPx c.start) LoopStart wave.height
                                            , cursor (toPx c.end) LoopEnd wave.height
                                            , cursor (toPx c.offset) StartOffset wave.height
                                            ]
                                                ++ (case wave.sel of
                                                        Just points ->
                                                            [ selection points Nothing <| rgba 0.3 0.3 0.3 0.3 ]

                                                        Nothing ->
                                                            []
                                                   )

                                        CollarDiv c ->
                                            [ selection ( toPx 0, toPx c.start ) Nothing <| rgba 0.5 0.5 0.5 0.5
                                            , selection ( toPx c.end, toPx 1 ) Nothing <| rgba 0.5 0.5 0.5 0.5
                                            , selection ( toPx c.start, toPx c.end ) (Just IWaveSel) <| rgba 0 0 0 0
                                            , cursor (toPx c.start) LoopStart wave.height
                                            , cursor (toPx c.end) LoopEnd wave.height
                                            ]
                                                ++ List.indexedMap (\i div -> cursor (toPx div) (Divide i) wave.height) c.divs
                               )

                    Nothing ->
                        []
               )
        )
    <|
        html <|
            canvas
                [ Attr.hidden <| mayCursors == Nothing
                , Attr.id canvasId
                , Attr.width wave.size
                , Attr.height wave.height
                ]
                []


cursor : Int -> Cursor -> Int -> Attribute (Interact.Msg Interactable zone)
cursor pos cur h =
    let
        handle attrs =
            inFront <|
                el
                    ([ htmlAttribute <| Attr.style "cursor" "grab"
                     , height <| px <| border * 8
                     , width <| px <| border * 8
                     , Border.rounded <| border * 4
                     , Bg.color <| rgb 0 0 0
                     , moveLeft <| toFloat <| border * 4
                     ]
                        ++ attrs
                    )
                    none
    in
    inFront <|
        el
            ([ htmlAttribute <| Attr.style "cursor" "ew-resize"
             , Border.width <| border
             , height fill
             , moveRight <| toFloat <| pos - border
             ]
                ++ (case cur of
                        LoopStart ->
                            [ handle [ alignTop, moveUp <| toFloat border ] ]

                        LoopEnd ->
                            [ handle [ alignBottom, moveDown <| toFloat border ] ]

                        StartOffset ->
                            [ handle [ centerY, moveDown <| toFloat h / 4 ]
                            , handle [ centerY, moveUp <| toFloat h / 4 ]
                            ]

                        Divide _ ->
                            [ handle [ alignTop, moveUp <| toFloat border ]
                            , handle [ alignBottom, moveDown <| toFloat border ]
                            ]
                   )
                ++ (List.map htmlAttribute <| Interact.draggableEvents <| IWaveCursor cur)
            )
            none


selection : ( Int, Int ) -> Maybe Interactable -> Color -> Attribute (Interact.Msg Interactable zone)
selection ( a, b ) mayInter color =
    inFront <|
        el
            ([ Bg.color color
             , width <| px (b - a + 1)
             , height fill
             , moveRight <| toFloat <| a
             ]
                ++ (Maybe.withDefault [] <|
                        Maybe.map
                            (List.map htmlAttribute
                                << (::) (Attr.style "cursor" "move")
                                << Interact.draggableEvents
                            )
                            mayInter
                   )
            )
            none
