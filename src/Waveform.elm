port module Waveform exposing (..)

import DOM
import Dict exposing (Dict)
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


port requestSoundDraw : SoundRequest -> Cmd msg


port soundDrawn : (D.Value -> msg) -> Sub msg


canvasId : String
canvasId =
    "waveform"


mapCanvasId : String
mapCanvasId =
    "waveformMap"


border : Int
border =
    2


waveHeight : Int
waveHeight =
    150


mapHeight : Int
mapHeight =
    50


type alias Waveform =
    { size : Int
    , drawn : Drawing
    , sel : Maybe ( Float, Float )
    , zoomFactor : Float
    , startPercent : Float
    }


type alias SoundRequest =
    { soundPath : String
    , zoomFactor : Float
    , startPercent : Float
    , wait : Bool
    , waveformMap : Bool
    }


type Drawing
    = None
    | SoundDrawn String
    | Pending String


init : Waveform
init =
    { size = 1000
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


type Mark
    = MStart
    | MEnd
    | MLoopStart
    | MLoopEnd
    | MDiv Int


type Msg
    = GotSize Int
    | ChgSound String
    | ChgView Float Float -- Zoom, Center percent
    | MoveStartPercent Int
    | MoveEndPercent Int
    | MoveView Int
    | CenterOn Mark (Maybe Cursors)
    | ZoomPoint Float Float -- wheelDelta, xOffset
    | Zoom Bool
    | GotDrawn (Result D.Error String)
    | Select ( Float, Float )
    | MoveSel Float
    | CancelSel


update : Msg -> Waveform -> ( Waveform, Cmd Msg )
update msg wave =
    case msg of
        GotSize size ->
            ( { wave | size = size - 2 * border }
            , requestFullDraw True wave
            )

        ChgSound name ->
            if wave.drawn == SoundDrawn name then
                ( { wave | zoomFactor = init.zoomFactor, startPercent = init.startPercent }, Cmd.none )

            else
                let
                    newWave =
                        { wave | drawn = Pending name, zoomFactor = init.zoomFactor, startPercent = init.startPercent }
                in
                ( newWave
                , requestFullDraw False newWave
                )

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

        MoveStartPercent d ->
            let
                viewPercent =
                    1 / wave.zoomFactor

                percentD =
                    mapPxToSoundPercent wave d

                f =
                    1 / (viewPercent - percentD)

                newWave =
                    { wave
                        | startPercent = clamp 0 (1 - 1 / f) wave.startPercent + percentD
                        , zoomFactor = f
                    }
            in
            ( newWave, requestRedraw newWave )

        MoveEndPercent d ->
            let
                viewPercent =
                    1 / wave.zoomFactor

                percentD =
                    mapPxToSoundPercent wave d

                newWave =
                    { wave
                        | zoomFactor = 1 / (viewPercent + percentD)
                    }
            in
            ( newWave, requestRedraw newWave )

        MoveView d ->
            update (ChgView wave.zoomFactor <| wave.startPercent + pxToSoundDist wave d) wave

        CenterOn cur mayCursors ->
            let
                up pos =
                    update (ChgView wave.zoomFactor <| pos - 0.5 / wave.zoomFactor) wave
            in
            case cur of
                MStart ->
                    up 0

                MEnd ->
                    up 1

                _ ->
                    case mayCursors of
                        Just (Sound { start, end, offset }) ->
                            case cur of
                                MLoopStart ->
                                    up start

                                MLoopEnd ->
                                    up end

                                MDiv _ ->
                                    up offset

                                _ ->
                                    ( wave, Cmd.none )

                        Just (CollarDiv { start, end, divs }) ->
                            case cur of
                                MLoopStart ->
                                    up start

                                MLoopEnd ->
                                    up end

                                MDiv i ->
                                    Maybe.withDefault ( wave, Cmd.none ) <|
                                        Maybe.map up <|
                                            List.head <|
                                                List.drop i divs

                                _ ->
                                    ( wave, Cmd.none )

                        Nothing ->
                            ( wave, Cmd.none )

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

        Zoom b ->
            let
                factor =
                    if b then
                        0.9

                    else
                        1.1

                f =
                    clamp 1 (1 / 0) <| wave.zoomFactor / factor

                a =
                    clamp 0 (1 - 1 / f) <| wave.startPercent - (1 / f - 1 / wave.zoomFactor) / 2

                newWave =
                    { wave | zoomFactor = f, startPercent = a }
            in
            ( newWave, requestRedraw newWave )

        GotDrawn res ->
            case res of
                Ok soundName ->
                    case wave.drawn of
                        Pending name ->
                            if name == soundName then
                                ( { wave | drawn = SoundDrawn name }, Cmd.none )

                            else
                                ( wave, requestFullDraw False wave )

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


keyCodeToShortcut : Maybe Cursors -> Dict String Msg
keyCodeToShortcut mayC =
    let
        msg mark =
            CenterOn mark mayC
    in
    Dict.fromList <|
        ( "Backquote", msg MStart )
            :: List.concatMap (\( k1, k2, m ) -> [ ( k1, m ), ( k2, m ) ])
                [ ( "Digit0", "Numpad0", msg MStart )
                , ( "Digit9", "Numpad9", msg MEnd )
                , ( "Digit1", "Numpad1", msg MLoopStart )
                , ( "Digit2", "Numpad2", msg <| MDiv 0 )
                , ( "Digit3", "Numpad3", msg <| MDiv 1 )
                , ( "Digit4", "Numpad4", msg <| MDiv 2 )
                , ( "Digit5", "Numpad5", msg <| MDiv 3 )
                , ( "Digit6", "Numpad6", msg <| MDiv 4 )
                , ( "Digit7", "Numpad7", msg <| MDiv 5 )
                , ( "Digit8", "Numpad8", msg MLoopEnd )
                ]


keyCodeToDirection : Dict String Msg
keyCodeToDirection =
    Dict.fromList
        [ ( "KeyG", Zoom False )
        , ( "KeyH", Zoom True )
        , ( "KeyB", MoveView -20 )
        , ( "KeyN", MoveView 20 )
        ]


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
        showMini =
            mayCursors /= Nothing && wave.zoomFactor /= 1

        curs pos =
            let
                p =
                    soundPercentToViewPercent wave pos
            in
            if p >= 0 && p <= 1 then
                List.singleton << (cursor waveHeight <| viewPercentToPx wave p)

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

        toMapPx =
            soundPercentToMapPx wave

        miniCurs =
            cursor mapHeight << toMapPx

        miniSel tup foo bar =
            selection (Tuple.mapBoth toMapPx toMapPx tup) foo bar

        whiteSel =
            rgba 0 0 0 0

        greySel =
            rgba 0.5 0.5 0.5 0.5

        darkGreySel =
            rgba 0.3 0.3 0.3 0.3
    in
    column
        [ Bg.color <| rgb 1 1 1
        , alignBottom
        ]
        [ el
            (case ( showMini, mayCursors ) of
                ( True, Just cursors ) ->
                    let
                        p1 =
                            wave.startPercent

                        p2 =
                            wave.startPercent + pxToSoundDist wave wave.size
                    in
                    List.map (mapAttribute wrapInter) <|
                        (List.map htmlAttribute <| Interact.dragSpaceEvents interState ZWaveMap)
                            ++ (case cursors of
                                    Sound c ->
                                        [ miniSel ( 0, c.start ) Nothing greySel
                                        , miniSel ( c.end, 1 ) Nothing greySel
                                        , miniSel ( p1, p2 ) (Just IWaveMapSel) whiteSel
                                        , miniCurs c.start <| LoopStart Mini
                                        , miniCurs c.end <| LoopEnd Mini
                                        , miniCurs c.offset <| StartOffset Mini
                                        ]

                                    CollarDiv c ->
                                        [ miniSel ( 0, c.start ) Nothing greySel
                                        , miniSel ( c.end, 1 ) Nothing greySel
                                        , miniSel ( p1, p2 ) (Just IWaveMapSel) whiteSel
                                        , miniCurs c.start <| LoopStart Mini
                                        , miniCurs c.end <| LoopEnd Mini
                                        ]
                                            ++ List.indexedMap (\i div -> miniCurs div <| Divide i Mini) c.divs
                               )
                            ++ [ miniCurs p1 ViewStart
                               , miniCurs p2 ViewEnd
                               ]

                _ ->
                    []
            )
          <|
            html <|
                canvas
                    [ Attr.hidden <| not showMini
                    , Attr.id mapCanvasId
                    , Attr.width <| wave.size + 2 * border
                    , Attr.height mapHeight
                    ]
                    []
        , el
            (case mayCursors of
                Just cursors ->
                    ((htmlAttribute << Attr.map wrapMsg) <|
                        Events.on "wheel" <|
                            D.map3 (\deltaY clientX rect -> ZoomPoint deltaY <| clientX - rect.left - toFloat border)
                                (D.field "deltaY" D.float)
                                (D.field "clientX" D.float)
                                (DOM.currentTarget DOM.boundingClientRect)
                    )
                        :: [ Border.color <| rgb 0 0 0
                           , Border.width border
                           ]
                        ++ (List.map (mapAttribute wrapInter) <|
                                (List.map htmlAttribute <| Interact.dragSpaceEvents interState ZWave)
                                    ++ (case cursors of
                                            Sound c ->
                                                List.concat
                                                    [ sel ( 0, c.start ) Nothing greySel
                                                    , sel ( c.end, 1 ) Nothing greySel
                                                    , sel ( c.start, c.end ) (Just IWaveSel) whiteSel
                                                    , curs c.start <| LoopStart Main
                                                    , curs c.end <| LoopEnd Main
                                                    , curs c.offset <| StartOffset Main
                                                    , case wave.sel of
                                                        Just points ->
                                                            sel points Nothing darkGreySel

                                                        Nothing ->
                                                            []
                                                    ]

                                            CollarDiv c ->
                                                List.concat <|
                                                    [ sel ( 0, c.start ) Nothing greySel
                                                    , sel ( c.end, 1 ) Nothing greySel
                                                    , sel ( c.start, c.end ) (Just IWaveSel) whiteSel
                                                    , curs c.start <| LoopStart Main
                                                    , curs c.end <| LoopEnd Main
                                                    ]
                                                        ++ List.indexedMap (\i div -> curs div (Divide i Main)) c.divs
                                       )
                           )

                Nothing ->
                    []
            )
          <|
            html <|
                canvas
                    [ Attr.hidden <| mayCursors == Nothing
                    , Attr.id canvasId
                    , Attr.width wave.size
                    , Attr.height waveHeight
                    ]
                    []
        ]


requestHelper : Bool -> Bool -> { a | zoomFactor : Float, startPercent : Float, drawn : Drawing } -> Cmd msg
requestHelper full wait wave =
    let
        cmd soundName =
            requestSoundDraw <| SoundRequest soundName wave.zoomFactor wave.startPercent wait full
    in
    case wave.drawn of
        SoundDrawn name ->
            cmd name

        Pending name ->
            cmd name

        None ->
            Cmd.none


requestRedraw : { a | zoomFactor : Float, startPercent : Float, drawn : Drawing } -> Cmd msg
requestRedraw =
    requestHelper False False


requestFullDraw : Bool -> { a | zoomFactor : Float, startPercent : Float, drawn : Drawing } -> Cmd msg
requestFullDraw =
    requestHelper True


cursor : Int -> Int -> Cursor -> Attribute (Interact.Msg Interactable zone)
cursor h pos cur =
    let
        size =
            if h >= 100 then
                8

            else
                6

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
                        LoopStart _ ->
                            [ handle [ alignTop, moveUp <| toFloat border ] ]

                        LoopEnd _ ->
                            [ handle [ alignBottom, moveDown <| toFloat border ] ]

                        StartOffset _ ->
                            [ handle [ centerY, moveDown <| toFloat h / 4 ]
                            , handle [ centerY, moveUp <| toFloat h / 4 ]
                            ]

                        Divide _ _ ->
                            [ handle [ alignTop, moveUp <| toFloat border ]
                            , handle [ alignBottom, moveDown <| toFloat border ]
                            ]

                        _ ->
                            []
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


soundPercentToMapPx : Waveform -> Float -> Int
soundPercentToMapPx w p =
    round <| p * (toFloat <| w.size + 2 * border)


mapPxToSoundPercent : Waveform -> Int -> Float
mapPxToSoundPercent w px =
    toFloat px / toFloat (w.size + 2 * border)
