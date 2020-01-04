port module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Coll exposing (Coll, Id)
import Collar
import Content
import Dict exposing (Dict)
import Doc exposing (Doc)
import Editor.Mobile as MEditor exposing (Interactable(..))
import Element exposing (..)
import Element.Background as Bg
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as D
import Keys
import Mobile exposing (Mobeel)
import Result exposing (Result)
import Set exposing (Set)
import Sound exposing (Sound)
import Url exposing (Url)
import Wheel


port loadSound : String -> Cmd msg


port soundLoaded : (D.Value -> msg) -> Sub msg



-- TODO refactor existing Debug.log with "key" value
-- TODO check bug visibility hidden not emitted on window change but on tab change
-- TODO check msg or Msg in types, if unused, maybe replace by x
-- TODO clean all module exposings decl
-- TODO is "No error handling in update, everything comes Checked before" is a good pattern ?
-- TODO change all debug and silent edge or fail (_/NOOP) to debug.log
-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        , onUrlRequest = always NOOP
        , onUrlChange = always NOOP
        }



-- MODEL


type alias Model =
    { connected : Bool
    , currentUrl : Url.Url
    , soundList : Set String
    , loadedSoundList : List Sound
    , savesList : Set String
    , doc : Doc
    , screenSize : ScreenSize
    , fileExplorerTab : ExTab
    , mode : Mode
    , keys : Keys.State
    }


type alias ScreenSize =
    { width : Int, height : Int }


type ExTab
    = Sounds
    | Loaded
    | Saves


init : ScreenSize -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init screen url _ =
    ( Model
        False
        url
        Set.empty
        []
        Set.empty
        (Doc.init <| Just url)
        screen
        Sounds
        NoMode
        Keys.init
    , fetchSoundList url
    )



-- UPDATE


type Msg
    = GotSoundList (Result Http.Error String)
    | RequestSoundList
    | RequestSoundLoad String
    | RequestSavesList
    | RequestSaveLoad String
    | GotSavesList (Result Http.Error String)
    | GotLoadedFile String (Result Http.Error Mobeel)
    | SoundLoaded (Result D.Error Sound)
    | ChangedExplorerTab ExTab
    | ChangedMode Mode
    | GotScreenSize ScreenSize
    | DocMsg Doc.Msg
    | KeysMsg Keys.Msg
    | NOOP


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSoundList result ->
            case result of
                Ok stringList ->
                    ( { model
                        | soundList = Set.union model.soundList <| Set.fromList <| String.split "\\" stringList
                        , connected = True
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | connected = False }, Cmd.none )

        GotSavesList result ->
            case result of
                Ok stringList ->
                    ( { model
                        | savesList = Set.union model.savesList <| Set.fromList <| String.split "\\" stringList
                        , connected = True
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | connected = False, savesList = Set.empty }, Cmd.none )

        GotLoadedFile name result ->
            case result of
                Ok m ->
                    let
                        -- TODO Report bug, forced to map .wheel and {wheel} because {a | wheel} doesn’t work
                        -- TODO Report bug, can’t define recursive function if it has no argument (f l = f l instead of f = f)
                        loadList l =
                            List.concatMap
                                (\wheel ->
                                    case Wheel.getContent { wheel = wheel } of
                                        Content.S s ->
                                            if List.member s model.loadedSoundList then
                                                []

                                            else
                                                [ Tuple.second <|
                                                    update
                                                        (RequestSoundLoad <| Sound.toString s)
                                                        model
                                                ]

                                        Content.M mob ->
                                            loadList <| List.map .wheel <| Coll.values mob.gears

                                        Content.C col ->
                                            loadList <| List.map .wheel <| Collar.getBeads col
                                )
                                l

                        newModel =
                            { model | connected = True }

                        subMsg =
                            case newModel.mode of
                                Capsuling ->
                                    Doc.AddContent <| Content.M m

                                _ ->
                                    Doc.Loaded m name

                        ( mod, cmd ) =
                            update (DocMsg subMsg) newModel
                    in
                    ( mod, Cmd.batch <| cmd :: (loadList <| List.map .wheel <| Coll.values m.gears) )

                Err (Http.BadBody err) ->
                    Debug.log err ( model, Cmd.none )

                Err _ ->
                    ( { model | connected = False }, Cmd.none )

        RequestSoundList ->
            ( model, fetchSoundList model.currentUrl )

        RequestSoundLoad n ->
            -- TODO handle no response
            ( model
            , if Set.member n model.soundList then
                loadSound n

              else
                Cmd.none
            )

        RequestSavesList ->
            ( model, fetchSavesList model.currentUrl )

        RequestSaveLoad n ->
            -- TODO handle no response
            ( model
            , if Set.member n model.savesList then
                fetchSaveFile model.currentUrl n

              else
                Cmd.none
            )

        SoundLoaded res ->
            case res of
                Result.Err e ->
                    Debug.log (D.errorToString e) ( model, Cmd.none )

                Result.Ok s ->
                    ( { model | loadedSoundList = s :: model.loadedSoundList }, Cmd.none )

        ChangedExplorerTab tab ->
            ( { model | fileExplorerTab = tab }, Cmd.none )

        -- FIXME Code smell?
        ChangedMode mode ->
            case mode of
                MobileMode subMode ->
                    update (DocMsg <| Doc.MobileMsg <| MEditor.ChangedMode subMode) { model | mode = mode }

                _ ->
                    let
                        ( newModel, cmds ) =
                            update (DocMsg <| Doc.MobileMsg <| MEditor.ChangedMode MEditor.Normal) model
                    in
                    case mode of
                        Capsuling ->
                            ( { newModel | mode = Capsuling, fileExplorerTab = Saves }, cmds )

                        _ ->
                            ( { newModel | mode = mode }, cmds )

        GotScreenSize size ->
            ( { model | screenSize = size }, Cmd.none )

        DocMsg subMsg ->
            let
                ( doc, cmd ) =
                    Doc.update subMsg model.doc
            in
            case subMsg of
                Doc.MobileMsg (MEditor.ChangedMode (MEditor.ChangeSound _)) ->
                    ( { model | doc = doc, fileExplorerTab = Loaded }, Cmd.map DocMsg cmd )

                _ ->
                    ( { model | doc = doc }, Cmd.map DocMsg cmd )

        KeysMsg subMsg ->
            let
                ( state, event ) =
                    Keys.update subMsg model.keys
            in
            case event of
                Keys.Hold hold ->
                    case List.filterMap (\code -> Dict.get code keyCodeToMode) <| Set.toList hold of
                        [ only ] ->
                            update (ChangedMode only) { model | keys = state }

                        _ ->
                            update (ChangedMode NoMode) { model | keys = state }

                Keys.Press code ->
                    case Dict.get code keyCodeToShortcut of
                        Just press ->
                            let
                                ( doc, cmd ) =
                                    Doc.update (Doc.KeyPressed press) model.doc
                            in
                            ( { model | keys = state, doc = doc }, Cmd.map DocMsg cmd )

                        Nothing ->
                            ( { model | keys = state }, Cmd.none )

        NOOP ->
            ( model, Cmd.none )



-- SUBS


subs : Model -> Sub Msg
subs { doc } =
    Sub.batch <|
        [ soundLoaded (SoundLoaded << D.decodeValue Sound.decoder)
        , BE.onResize (\w h -> GotScreenSize { width = w, height = h })
        ]
            ++ List.map (Sub.map DocMsg) (Doc.subs doc)
            ++ List.map (Sub.map KeysMsg) Keys.subs


type Mode
    = MobileMode MEditor.Mode -- FIXME Two sources of truth
    | Capsuling
    | NoMode


keyCodeToMode : Dict String Mode
keyCodeToMode =
    Dict.fromList <|
        ( "KeyE", Capsuling )
            :: List.map (Tuple.mapSecond MobileMode) MEditor.keyCodeToMode


keyCodeToShortcut : Dict String Doc.Shortcut
keyCodeToShortcut =
    Dict.fromList
        [ ( "KeyZ", Doc.Tool 1 )
        , ( "KeyX", Doc.Tool 2 )
        , ( "KeyC", Doc.Tool 3 )
        , ( "Space", Doc.Play )
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title =
        "Gears !"
            ++ (if model.connected then
                    ""

                else
                    " - DISCONNECTED"
               )
    , body =
        [ layout [] <|
            row [ height <| px model.screenSize.height, width <| px model.screenSize.width ]
                [ viewFileExplorer model
                , Element.map DocMsg <| Doc.view model.doc
                ]
        ]
    }


viewFileExplorer : Model -> Element Msg
viewFileExplorer model =
    let
        bgColor =
            if model.mode == Capsuling then
                rgb 0.2 0.2 0.8

            else
                rgb 0.5 0.5 0.5
    in
    column [ height fill, Bg.color bgColor, Font.color (rgb 1 1 1), Font.size 16, spacing 20, padding 10 ] <|
        ([ row [ Font.size 14, spacing 20 ]
            [ Input.button
                (if model.fileExplorerTab == Sounds then
                    [ padding 5, Bg.color (rgb 0.2 0.2 0.2) ]

                 else
                    [ padding 5 ]
                )
                { label = text "Sons"
                , onPress = Just <| ChangedExplorerTab Sounds
                }
            , Input.button
                (if model.fileExplorerTab == Loaded then
                    [ padding 5, Bg.color (rgb 0.1 0.1 0.1) ]

                 else
                    [ padding 5 ]
                )
                { label = text "Chargés"
                , onPress = Just <| ChangedExplorerTab Loaded
                }
            , Input.button
                (if model.fileExplorerTab == Saves then
                    [ padding 5, Bg.color (rgb 0.1 0.1 0.1) ]

                 else
                    [ padding 5 ]
                )
                { label = text "Saves"
                , onPress = Just <| ChangedExplorerTab Saves
                }
            ]
         ]
            ++ (case model.fileExplorerTab of
                    Sounds ->
                        viewSounds model

                    Loaded ->
                        viewLoaded model

                    Saves ->
                        viewSaveFiles model
               )
        )


viewSounds : Model -> List (Element Msg)
viewSounds model =
    [ column [ width fill, height <| fillPortion 2, spacing 20, scrollbarY ]
        [ Input.button
            [ Font.color <|
                if model.connected then
                    rgb 0 0 0

                else
                    rgb 1 0 0
            ]
            { onPress = Just RequestSoundList
            , label = text "Actualiser"
            }
        , column [ width fill, height <| fillPortion 1, spacing 5, padding 2, scrollbarY ] <|
            (List.map
                (\s ->
                    el
                        [ onClick (RequestSoundLoad s)
                        , Font.color <|
                            if List.any ((==) s) <| List.map Sound.toString model.loadedSoundList then
                                rgb 0.2 0.8 0.2

                            else
                                rgb 1 1 1
                        ]
                        (text s)
                )
             <|
                Set.toList model.soundList
            )
        ]
    ]


viewLoaded : Model -> List (Element Msg)
viewLoaded model =
    [ column [ width fill, height <| fillPortion 3, spacing 10, padding 2, scrollbarY ] <|
        List.map soundView model.loadedSoundList
    ]


soundView : Sound -> Element Msg
soundView s =
    el
        [ onClick <| DocMsg <| Doc.SoundClicked s ]
        (text (Sound.toString s))


viewSaveFiles : Model -> List (Element Msg)
viewSaveFiles model =
    [ column [ height <| fillPortion 1, width fill, spacing 20 ]
        [ Input.button
            [ Font.color <|
                if model.connected then
                    rgb 0 0 0

                else
                    rgb 1 0 0
            ]
            { onPress = Just RequestSavesList
            , label = text "Actualiser"
            }
        , column [ width fill, spacing 5, padding 2, scrollbarY ] <|
            (List.map (\s -> el [ onClick (RequestSaveLoad s) ] (text <| cutGearsExtension s)) <|
                Set.toList model.savesList
            )
        ]
    ]



-- HTTP


fetchSoundList : Url.Url -> Cmd Msg
fetchSoundList url =
    Http.get
        { url = Url.toString { url | path = "/soundList" }
        , expect = Http.expectString GotSoundList
        }


fetchSavesList : Url.Url -> Cmd Msg
fetchSavesList url =
    Http.get
        { url = Url.toString { url | path = "/savesList" }
        , expect = Http.expectString GotSavesList
        }


fetchSaveFile : Url.Url -> String -> Cmd Msg
fetchSaveFile url name =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Cache-Control" "no-cache" ]
        , url = Url.toString { url | path = "/saves/" ++ name }
        , body = Http.emptyBody
        , expect = Http.expectJson (GotLoadedFile <| cutGearsExtension name) Mobile.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


cutGearsExtension : String -> String
cutGearsExtension =
    String.dropRight 6
