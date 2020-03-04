port module Waveform exposing (..)

import Editor.Interacting exposing (..)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Html exposing (canvas)
import Html.Attributes as Attr
import Interact
import Json.Decode as D



-- TODO Maybe better to compute min max rms in js but draw in elm with adequate lib


port requestSoundDraw : String -> Cmd msg


port soundDrawn : (D.Value -> msg) -> Sub msg


canvasId : String
canvasId =
    "waveform"


border : Int
border =
    2


type alias Waveform =
    { size : Int
    , drawn : Drawing
    }


type Drawing
    = None
    | SoundDrawn String
    | Pending String


init : Waveform
init =
    { size = 1000
    , drawn = None
    }


type Msg
    = GotSize Int
    | ChgSound String
    | GotDrawn (Result D.Error String)


update : Msg -> Waveform -> ( Waveform, Cmd Msg )
update msg wave =
    case msg of
        GotSize size ->
            ( { wave | size = size - 2 * border }
            , case wave.drawn of
                SoundDrawn s ->
                    requestSoundDraw s

                _ ->
                    Cmd.none
            )

        ChgSound s ->
            if wave.drawn == SoundDrawn s then
                ( wave, Cmd.none )

            else
                ( { wave | drawn = Pending s }, requestSoundDraw s )

        GotDrawn res ->
            case res of
                Ok str ->
                    case wave.drawn of
                        Pending s ->
                            if s == str then
                                ( { wave | drawn = SoundDrawn s }, Cmd.none )

                            else
                                ( wave, requestSoundDraw s )

                        _ ->
                            ( wave, Cmd.none )

                Err err ->
                    Debug.log ("Error while drawing " ++ D.errorToString err) ( wave, Cmd.none )


sub : Sub Msg
sub =
    soundDrawn (GotDrawn << D.decodeValue D.string)


view :
    Bool
    -> Waveform
    -> { offset : Float, start : Float, end : Float }
    -> Interact.State Interactable Zone
    -> (Interact.Msg Interactable Zone -> msg)
    -> Element msg
view visible wf cursors interState wrapInter =
    let
        toPx =
            round << ((*) <| toFloat wf.size)
    in
    el
        (if visible then
            (List.map (htmlAttribute << Attr.map wrapInter) <| Interact.dragSpaceEvents interState ZWave)
                ++ [ Border.color <| rgb 0 0 0
                   , Border.width border
                   , Bg.color <| rgb 1 1 1
                   , alignBottom
                   ]
                ++ List.map (mapAttribute wrapInter)
                    [ selection ( toPx 0, toPx cursors.start ) Nothing <| rgba 0.5 0.5 0.5 0.5
                    , selection ( toPx cursors.end, toPx 1 ) Nothing <| rgba 0.5 0.5 0.5 0.5
                    , selection ( toPx cursors.start, toPx cursors.end ) (Just IWaveSel) <| rgba 0 0 0 0
                    , cursor (toPx cursors.start) LoopStart
                    , cursor (toPx cursors.end) LoopEnd
                    , cursor (toPx cursors.offset) StartOffset
                    ]

         else
            []
        )
    <|
        html <|
            canvas
                [ Attr.hidden <| not visible
                , Attr.id canvasId
                , Attr.width wf.size
                ]
                []


cursor : Int -> Cursor -> Attribute (Interact.Msg Interactable zone)
cursor pos cur =
    inFront <|
        el
            ([ htmlAttribute <| Attr.style "cursor" "ew-resize"
             , Border.width <| border
             , height fill
             , moveRight <| toFloat <| pos - border
             , inFront <|
                el
                    ([ htmlAttribute <| Attr.style "cursor" "grab"
                     , height <| px <| border * 8
                     , width <| px <| border * 8
                     , Border.rounded <| border * 4
                     , Bg.color <| rgb 0 0 0
                     , moveLeft <| toFloat <| border * 4
                     ]
                        ++ (case cur of
                                LoopStart ->
                                    [ alignTop, moveUp <| toFloat border ]

                                LoopEnd ->
                                    [ alignBottom, moveDown <| toFloat border ]

                                StartOffset ->
                                    [ centerY ]
                           )
                    )
                    none
             ]
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
