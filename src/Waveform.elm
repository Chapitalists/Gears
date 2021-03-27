port module Waveform exposing (..)

import DOM
import Editor.Interacting exposing (..)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Html exposing (canvas)
import Html.Attributes as Attr
import Html.Events as Events
import Interact
import Json.Decode as D



-- TODO Maybe better to compute min max rms in js but draw in elm with adequate lib


port requestSoundDraw : SoundView -> Cmd msg


port soundDrawn : (D.Value -> msg) -> Sub msg


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
    , sel : Maybe ( Float, Float )
    , zoomFactor : Float
    , startPercent : Float
    }


type alias SoundView =
    { soundName : String
    , zoomFactor : Float
    , startPercent : Float
    }


type Drawing
    = None
    | SoundDrawn String
    | Pending String


init : Waveform
init =
    { size = 1000
    , height = 150
    , drawn = None
    , sel = Nothing
    , zoomFactor = 1
    , startPercent = 0
    }


getSelPercents : Waveform -> Maybe ( Float, Float )
getSelPercents { sel } =
    sel


isDrawn : Waveform -> String -> Bool
isDrawn { drawn } name =
    case drawn of
        SoundDrawn soundName ->
            name == soundName

        _ ->
            False


type Msg
    = GotSize Int
    | ChgSound String
    | ChgView Float Float -- Zoom, Center percent
    | ZoomPoint Float Float -- wheelDelta, xOffset
    | GotDrawn (Result D.Error String)
    | Select ( Float, Float )
    | MoveSel Float
    | CancelSel


update : Msg -> Waveform -> ( Waveform, Cmd Msg )
update msg wave =
    case msg of
        GotSize size ->
            ( { wave | size = size - 2 * border }
            , case wave.drawn of
                SoundDrawn name ->
                    requestSoundDraw <| SoundView name wave.zoomFactor wave.startPercent

                _ ->
                    Cmd.none
            )

        ChgSound name ->
            let
                f =
                    init.zoomFactor

                a =
                    init.startPercent

                chgRes =
                    ( { wave | drawn = Pending name, zoomFactor = f, startPercent = a }
                    , requestSoundDraw <| SoundView name f a
                    )
            in
            case wave.drawn of
                SoundDrawn soundName ->
                    if name == soundName then
                        ( wave, Cmd.none )

                    else
                        chgRes

                _ ->
                    chgRes

        ChgView factor start ->
            let
                f =
                    clamp 1 (1 / 0) factor

                a =
                    clamp 0 (1 - 1 / f) start

                newWave =
                    { wave | zoomFactor = f, startPercent = a }
            in
            ( newWave, requestRedraw newWave )

        ZoomPoint delta x ->
            let
                factor =
                    clamp 0.01 2 <| 1 + delta / 1000

                f =
                    wave.zoomFactor / factor

                d =
                    x / toFloat wave.size

                a =
                    wave.startPercent + d * (1 / factor - 1) / f
            in
            update (ChgView f a) wave

        GotDrawn res ->
            case res of
                Ok soundName ->
                    case wave.drawn of
                        Pending name ->
                            if name == soundName then
                                ( { wave | drawn = SoundDrawn name }, Cmd.none )

                            else
                                ( wave, requestSoundDraw <| SoundView name wave.zoomFactor wave.startPercent )

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
                tmpPV =
                    pxToSoundDist wave wave.size

                ( newWave, percentView ) =
                    if percentLength > tmpPV then
                        ( { wave
                            | zoomFactor = 1 / percentLength
                            , startPercent = clamp 0 (1 - percentLength) <| wave.startPercent - (percentLength - tmpPV) / 2
                          }
                        , percentLength
                        )

                    else
                        ( wave, tmpPV )

                half =
                    percentLength / 2

                ( vp1, vp2 ) =
                    ( newWave.startPercent, newWave.startPercent + percentView )

                safeCenter =
                    clamp (vp1 + half) (vp2 - half) <| pxToSoundPercent newWave <| round centerPx
            in
            ( { newWave | sel = Just ( safeCenter - half, safeCenter + half ) }, requestRedraw newWave )

        MoveSel pxD ->
            case wave.sel of
                Just ( p1, p2 ) ->
                    let
                        ( vp1, vp2 ) =
                            ( wave.startPercent, wave.startPercent + pxToSoundDist wave wave.size )

                        move =
                            (+) <| clamp -p1 (1 - p2) <| pxToSoundDist wave <| round pxD

                        ( newP1, newP2 ) =
                            ( move p1, move p2 )

                        newStartPercent =
                            if p1 < vp1 then
                                p1

                            else if p2 > vp2 then
                                vp1 + p2 - vp2

                            else
                                vp1

                        newWave =
                            { wave | sel = Just ( newP1, newP2 ), startPercent = newStartPercent }
                    in
                    ( newWave, requestRedraw newWave )

                Nothing ->
                    ( wave, Cmd.none )

        CancelSel ->
            ( { wave | sel = Nothing }, Cmd.none )


sub : Sub Msg
sub =
    soundDrawn (GotDrawn << D.decodeValue D.string)


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
        curs pos =
            let
                p =
                    soundPercentToViewPercent wave pos
            in
            if p >= 0 && p <= 1 then
                List.singleton << (cursor wave.height <| viewPercentToPx wave p)

            else
                always []

        sel tup foo bar =
            let
                clmapx =
                    viewPercentToPx wave << clamp 0 1 << soundPercentToViewPercent wave

                ( a, b ) =
                    Tuple.mapBoth clmapx clmapx tup
            in
            if a >= b then
                []

            else
                [ selection ( a, b ) foo bar ]
    in
    el
        ((htmlAttribute <|
            Attr.map wrapMsg <|
                Events.on "wheel" <|
                    D.map3 (\deltaY clientX rect -> ZoomPoint deltaY <| clientX - rect.left - toFloat border)
                        (D.field "deltaY" D.float)
                        (D.field "clientX" D.float)
                        (DOM.currentTarget DOM.boundingClientRect)
         )
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
                                            List.concat
                                                [ sel ( 0, c.start ) Nothing <| rgba 0.5 0.5 0.5 0.5
                                                , sel ( c.end, 1 ) Nothing <| rgba 0.5 0.5 0.5 0.5
                                                , sel ( c.start, c.end ) (Just IWaveSel) <| rgba 0 0 0 0
                                                , curs c.start LoopStart
                                                , curs c.end LoopEnd
                                                , curs c.offset StartOffset
                                                , case wave.sel of
                                                    Just points ->
                                                        sel points Nothing <| rgba 0.3 0.3 0.3 0.3

                                                    Nothing ->
                                                        []
                                                ]

                                        CollarDiv c ->
                                            List.concat <|
                                                [ sel ( 0, c.start ) Nothing <| rgba 0.5 0.5 0.5 0.5
                                                , sel ( c.end, 1 ) Nothing <| rgba 0.5 0.5 0.5 0.5
                                                , sel ( c.start, c.end ) (Just IWaveSel) <| rgba 0 0 0 0
                                                , curs c.start LoopStart
                                                , curs c.end LoopEnd
                                                ]
                                                    ++ List.indexedMap (\i div -> curs div (Divide i)) c.divs
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


requestRedraw : Waveform -> Cmd msg
requestRedraw wave =
    let
        cmd soundName =
            requestSoundDraw <| SoundView soundName wave.zoomFactor wave.startPercent
    in
    case wave.drawn of
        SoundDrawn name ->
            cmd name

        Pending name ->
            cmd name

        None ->
            Cmd.none


cursor : Int -> Int -> Cursor -> Attribute (Interact.Msg Interactable zone)
cursor h pos cur =
    let
        size =
            8

        handle attrs =
            inFront <|
                el
                    ([ htmlAttribute <| Attr.style "cursor" "grab"
                     , height <| px <| border * size
                     , width <| px <| border * size
                     , Border.rounded <| border * size // 2
                     , Bg.color <| rgb 0 0 0
                     , moveLeft <| toFloat <| border * size // 2
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


soundPercentToViewPercent : Waveform -> Float -> Float
soundPercentToViewPercent wave p =
    (p - wave.startPercent) * wave.zoomFactor


soundDistToViewDist : Waveform -> Float -> Float
soundDistToViewDist wave d =
    d * wave.zoomFactor


viewPercentToSoundPercent : Waveform -> Float -> Float
viewPercentToSoundPercent wave p =
    p / wave.zoomFactor + wave.startPercent


viewDistToSoundDist : Waveform -> Float -> Float
viewDistToSoundDist wave d =
    d / wave.zoomFactor


viewPercentToPx : Waveform -> Float -> Int
viewPercentToPx wave p =
    round <| p * toFloat wave.size


pxToViewPercent : Waveform -> Int -> Float
pxToViewPercent wave p =
    toFloat p / toFloat wave.size


soundPercentToPx : Waveform -> Float -> Int
soundPercentToPx w =
    soundPercentToViewPercent w >> viewPercentToPx w


pxToSoundPercent : Waveform -> Int -> Float
pxToSoundPercent w =
    pxToViewPercent w >> viewPercentToSoundPercent w


soundDistToPx : Waveform -> Float -> Int
soundDistToPx w =
    soundDistToViewDist w >> viewPercentToPx w


pxToSoundDist : Waveform -> Int -> Float
pxToSoundDist w =
    pxToViewPercent w >> viewDistToSoundDist w
