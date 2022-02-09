module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Doc exposing (Doc)
import Element exposing (..)
import Html.Attributes as Attr
import Library exposing (Library)
import Pack exposing (Pack)
import SoundCard exposing (SoundCard)
import Tools.Panel as P
import Url exposing (Url)
import Waveform exposing (Waveform)



-- TODO refactor existing Debug.log with "key" value
-- TODO check msg or Msg in types, if unused, maybe replace by x
-- TODO clean all module exposings decl
-- TODO is "No error handling in update, everything comes Checked before"
-- TODO    a good pattern ?
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
    P.Size


type alias Views =
    { lib : FullPanel
    , menu : P.ViewType
    , soundCard : P.ViewType
    }


type FullPanel
    = Full Int
    | Panel P.ViewType



--type Panel
--    = Library
--    | Properties
--    | Tools
--    | Wave
--    | SoundCard
--    | Pack
--    | Menu


initViews : Views
initViews =
    { lib = Panel P.Shown
    , menu = P.Shown
    , soundCard = P.Shown
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
    | ViewLibChg FullPanel
    | ViewMenuChg P.ViewType
    | ViewSoundChg P.ViewType
    | DocMsg Doc.Msg
    | LibMsg Library.Msg
    | SoundMsg SoundCard.Msg
    | NOOP


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotScreenSize size ->
            ( { model | screenSize = size }, Cmd.none )

        ViewLibChg fp ->
            let
                views =
                    model.views
            in
            ( { model | views = { views | lib = fp } }, Cmd.none )

        ViewMenuChg vt ->
            let
                views =
                    model.views
            in
            ( { model | views = { views | menu = vt } }, Cmd.none )

        ViewSoundChg vt ->
            let
                views =
                    model.views
            in
            ( { model | views = { views | soundCard = vt } }, Cmd.none )

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
            el
                ((case model.views.lib of
                    Panel vt ->
                        P.view
                            ( ViewLibChg << Panel
                            , map LibMsg <| Library.viewFileExplorer model.lib
                            )
                            P.Right
                            vt
                            model.screenSize

                    _ ->
                        Debug.todo "FullLib"
                 )
                    :: P.view
                        ( ViewMenuChg
                        , map DocMsg <| Doc.viewMenu model.doc
                        )
                        P.Top
                        model.views.menu
                        model.screenSize
                    :: P.view
                        ( ViewSoundChg
                        , column [] <|
                            (map SoundMsg <| SoundCard.view model.soundCard)
                                :: (List.map (map DocMsg) <| Doc.viewPlay model.doc)
                        )
                        P.Left
                        model.views.soundCard
                        model.screenSize
                    :: [ height <| px model.screenSize.height
                       , width <| px model.screenSize.width
                       , htmlAttribute <| Attr.style "flex-direction" "row"
                       ]
                )
                (map DocMsg <|
                    Doc.view model.doc
                )
        ]
    }
