port module SoundCard exposing
    ( Msg
    , SoundCard
    , init
    , stopMicRec
    , sub
    , update
    , view
    )

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input


port requestMicOpening : () -> Cmd msg


port gotMicOpened : (() -> msg) -> Sub msg


port requestMicRecStart : Bool -> Cmd msg


port gotMicRecState : (Bool -> msg) -> Sub msg


port requestMicRecStop : String -> Cmd msg


type SoundCard
    = Model Internals


type alias Internals =
    { micState : MicState
    , micRecName : String
    }


type MicState
    = Closed
    | Ready
    | AutoStart
    | Recording


init : SoundCard
init =
    Model
        { micState = Closed
        , micRecName = ""
        }


type Msg
    = RequestOpenMic
    | MicOpened
    | ClickStartMicRec
    | ClickStopMicRec
    | RecordState Bool
    | EnteredNewRecName String


update : Msg -> SoundCard -> ( SoundCard, Cmd Msg )
update msg (Model model) =
    (case msg of
        RequestOpenMic ->
            ( model
            , requestMicOpening ()
            )

        MicOpened ->
            ( { model | micState = Ready }
            , Cmd.none
            )

        ClickStartMicRec ->
            ( model
            , requestMicRecStart <| model.micState == AutoStart
            )

        ClickStopMicRec ->
            stopMicRec_ model

        RecordState running ->
            if running then
                ( { model | micState = Recording }
                , Cmd.none
                )

            else
                stopMicRec_ model

        EnteredNewRecName fileName ->
            ( { model | micRecName = fileName }
            , Cmd.none
            )
    )
        |> Tuple.mapFirst Model


sub : Sub Msg
sub =
    [ gotMicOpened <| always MicOpened
    , gotMicRecState RecordState
    ]
        |> Sub.batch


view : SoundCard -> Element Msg
view (Model model) =
    column [ width fill, spacing 20 ] <|
        case model.micState of
            Ready ->
                [ row []
                    [ Input.button []
                        { onPress =
                            if String.isEmpty model.micRecName then
                                Nothing

                            else
                                Just ClickStartMicRec
                        , label = text "Rec Mic"
                        }
                    ]
                , Input.text [ Font.color (rgb 0 0 0), paddingXY 5 0 ]
                    { text = model.micRecName
                    , placeholder =
                        Just <| Input.placeholder [] <| text "Nom du fichier"
                    , label = Input.labelHidden "New File Name"
                    , onChange = EnteredNewRecName
                    }
                ]

            AutoStart ->
                [ Input.button []
                    { onPress = Just ClickStartMicRec
                    , label = text "Start Now"
                    }
                , text model.micRecName
                ]

            Recording ->
                [ Input.button []
                    { onPress = Just <| ClickStopMicRec
                    , label = text "Stop Mic"
                    }
                , text model.micRecName
                ]

            Closed ->
                [ Input.button []
                    { onPress = Just RequestOpenMic
                    , label = text "Activer Micro"
                    }
                ]


stopMicRec_ : Internals -> ( Internals, Cmd msg )
stopMicRec_ model =
    ( { model | micState = Ready }
    , requestMicRecStop model.micRecName
    )


stopMicRec : SoundCard -> ( SoundCard, Cmd msg )
stopMicRec (Model model) =
    if model.micState == Recording then
        stopMicRec_ model
            |> Tuple.mapFirst Model

    else
        ( Model model, Cmd.none )
