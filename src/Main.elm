module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Doc exposing (Doc)
import Element exposing (..)
import Library exposing (Library)
import Pack exposing (Pack)
import SoundCard exposing (SoundCard)
import Url exposing (Url)
import Waveform exposing (Waveform)



-- TODO refactor existing Debug.log with "key" value
-- TODO check msg or Msg in types, if unused, maybe replace by x
-- TODO clean all module exposings decl
-- TODO is "No error handling in update, everything comes Checked before" is a good pattern ?
-- TODO change all debug and silent edge or fail (_/NOOP) to debug.log
-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = sub
        , view = view
        , onUrlRequest = always NOOP
        , onUrlChange = always NOOP
        }



-- MODEL


type alias Model =
    { screenSize : ScreenSize
    , views : Views
    , doc : Doc.Model
    , lib : Library
    , pack : Pack
    , soundCard : SoundCard
    }


type alias ScreenSize =
    { width : Int, height : Int }


type alias Views =
    { lib : ViewType
    , pack : ViewType
    , soundCard : ViewType
    , wave : Maybe ( Waveform, ViewType )
    }


type ViewType
    = Hidden
    | Panel
    | FullScreen


initViews =
    { lib = Panel
    , pack = Hidden
    , soundCard = Panel
    , wave = Nothing
    }


init : ScreenSize -> Url -> Nav.Key -> ( Model, Cmd Msg )
init screen url _ =
    let
        ( lib, libCmd ) =
            Library.init url
    in
    ( { screenSize = screen
      , views = initViews
      , doc = Doc.init <| Just url
      , lib = lib
      , pack = Pack.init
      , soundCard = SoundCard.init
      }
    , Cmd.map LibMsg libCmd
    )



-- UPDATE


type Msg
    = GotScreenSize ScreenSize
    | DocMsg Doc.Msg
    | LibMsg Library.Msg
    | SoundMsg SoundCard.Msg
    | NOOP


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotScreenSize size ->
            ( { model | screenSize = size }, Cmd.none )

        DocMsg subMsg ->
            let
                ( doc, cmd ) =
                    Doc.update subMsg model.doc
            in
            ( { model | doc = doc }, Cmd.map DocMsg cmd )

        LibMsg subMsg ->
            let
                ( l, cmd, wheel ) =
                    Library.update subMsg model.lib
            in
            ( { model | lib = l }, Cmd.map LibMsg cmd )

        SoundMsg subMsg ->
            let
                ( sc, cmd ) =
                    SoundCard.update subMsg model.soundCard
            in
            ( { model | soundCard = sc }, Cmd.map SoundMsg cmd )

        NOOP ->
            ( model, Cmd.none )



-- SUBS


sub : Model -> Sub Msg
sub { doc } =
    [ BE.onResize (\w h -> GotScreenSize { width = w, height = h })
    , Sub.map DocMsg <| Doc.sub doc
    , Sub.map LibMsg Library.sub
    , Sub.map SoundMsg SoundCard.sub
    ]
        |> Sub.batch



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Gears !"
    , body =
        [ layout [] <|
            row [ height <| px model.screenSize.height, width <| px model.screenSize.width ]
                [ Element.map LibMsg <| Library.viewFileExplorer model.lib
                , Element.map DocMsg <| Doc.view model.doc
                ]
        ]
    }
