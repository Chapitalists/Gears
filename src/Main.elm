port module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Coll exposing (Coll, Id)
import Data.Collar as Collar
import Data.Content as Content
import Data.Mobile as Mobile exposing (Mobeel)
import Data.Wheel as Wheel
import Dict exposing (Dict)
import Doc exposing (Doc)
import Editor.Interacting as Interacting
import Editor.Mobile as Editor
import Element exposing (..)
import Element.Background as Bg
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download as DL
import File.Select as Select
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Interact
import Json.Decode as D
import Keys
import NaturalOrdering as Natural
import PanSvg
import Result exposing (Result)
import Set exposing (Set)
import Sound exposing (Sound)
import Url exposing (Url)
import Url.Builder


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
    , soundList : Dict String Bool
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
        Dict.empty
        []
        Set.empty
        (Doc.init <| Just url)
        screen
        Sounds
        NoMode
        Keys.init
    , Cmd.batch [ fetchSoundList url, fetchSavesList url ]
    )



-- UPDATE


type Msg
    = GotSoundList (Result Http.Error String)
    | RequestSoundList
    | PreListening String Bool
    | RequestSoundLoad String
    | RequestSoundDownload String
    | RequestSavesList
    | RequestSaveLoad String
    | GotSavesList (Result Http.Error String)
    | GotLoadedFile String (Result Http.Error Mobeel)
    | SoundLoaded (Result D.Error Sound)
    | ClickedUpload
    | UploadSounds File (List File)
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
                        | soundList =
                            Dict.union model.soundList <|
                                Dict.fromList <|
                                    List.map (\str -> ( str, False )) <|
                                        String.split "\\" stringList
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
                                    case Wheel.getWheelContent wheel of
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
            , if Dict.member n model.soundList then
                loadSound n

              else
                Cmd.none
            )

        RequestSoundDownload n ->
            ( model
            , if Dict.member n model.soundList then
                DL.url <| Url.toString model.currentUrl ++ "sons/" ++ n

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

        ClickedUpload ->
            ( model, Select.files [ "audio/x-wav" ] UploadSounds )

        UploadSounds f lf ->
            ( model
            , Cmd.batch <|
                List.map
                    (\file ->
                        Http.post
                            { url = Url.toString model.currentUrl ++ "upSound"
                            , body =
                                Http.multipartBody
                                    [ Http.filePart "file" file
                                    ]
                            , expect = Http.expectWhatever <| always RequestSoundList
                            }
                    )
                    (f :: lf)
            )

        PreListening s p ->
            ( { model | soundList = Dict.update s (Maybe.map <| always p) model.soundList }, Cmd.none )

        ChangedExplorerTab tab ->
            ( { model | fileExplorerTab = tab }, Cmd.none )

        -- FIXME Code smell?
        ChangedMode mode ->
            case mode of
                EditorMode subMode ->
                    update (DocMsg <| Doc.MobileMsg <| Editor.ChangedMode subMode) { model | mode = mode }

                _ ->
                    let
                        ( newModel, cmds ) =
                            update (DocMsg <| Doc.MobileMsg <| Editor.ChangedMode Editor.Normal) model
                    in
                    case mode of
                        Capsuling ->
                            ( { newModel | mode = Capsuling, fileExplorerTab = Saves }, cmds )

                        Downloading ->
                            ( { newModel | mode = Downloading, fileExplorerTab = Sounds }, cmds )

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
                -- FIXME Absurd... Should be a commonMsg and common ChangedMode
                Doc.MobileMsg (Editor.ChangedMode (Editor.ChangeSound _)) ->
                    ( { model | doc = doc, fileExplorerTab = Loaded }, Cmd.map DocMsg cmd )

                _ ->
                    ( { model | doc = doc }, Cmd.map DocMsg cmd )

        -- TODO Should dispatch KeysMsg, not specific messages to each part, too big of a dependency
        KeysMsg subMsg ->
            let
                ( state, events ) =
                    Keys.update subMsg model.keys
            in
            List.foldl
                (\event ( m, c ) ->
                    case event of
                        Keys.Hold hold ->
                            case List.filterMap (\code -> Dict.get code keyCodeToMode) <| Set.toList hold of
                                [ only ] ->
                                    Tuple.mapSecond (\cm -> Cmd.batch [ cm, c ]) <| update (ChangedMode only) m

                                _ ->
                                    Tuple.mapSecond (\cm -> Cmd.batch [ cm, c ]) <| update (ChangedMode NoMode) m

                        Keys.Press code ->
                            case Dict.get code keyCodeToShortcut of
                                Just press ->
                                    let
                                        ( doc, cmd ) =
                                            Doc.update (Doc.KeyPressed press) m.doc
                                    in
                                    ( { m | doc = doc }, Cmd.batch [ c, Cmd.map DocMsg cmd ] )

                                Nothing ->
                                    ( m, c )

                        Keys.Repeat code ->
                            case Dict.get code keyCodeToDirection of
                                Just dir ->
                                    let
                                        ( doc, cmd ) =
                                            Doc.update (Doc.DirectionRepeat dir) m.doc
                                    in
                                    ( { m | doc = doc }, Cmd.batch [ c, Cmd.map DocMsg cmd ] )

                                Nothing ->
                                    ( m, c )
                )
                ( { model | keys = state }, Cmd.none )
                events

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
    = EditorMode Editor.Mode -- FIXME Second source of truth, not reliable
    | Capsuling
    | Downloading
    | NoMode


keyCodeToMode : Dict String Mode
keyCodeToMode =
    Dict.fromList <|
        [ ( "KeyE", Capsuling )
        , ( "KeyR", Downloading )
        ]
            ++ List.map (Tuple.mapSecond EditorMode) Doc.keyCodeToMode


keyCodeToShortcut : Dict String Doc.Shortcut
keyCodeToShortcut =
    Dict.fromList
        [ ( "KeyZ", Doc.Tool 1 )
        , ( "KeyX", Doc.Tool 2 )
        , ( "KeyC", Doc.Tool 3 )
        , ( "Space", Doc.Play )
        , ( "ArrowLeft", Doc.Left )
        , ( "ArrowRight", Doc.Right )
        , ( "Backspace", Doc.Suppr )
        , ( "Delete", Doc.Suppr )
        , ( "KeyT", Doc.Pack )
        ]


keyCodeToDirection : Dict String PanSvg.Direction
keyCodeToDirection =
    Dict.fromList
        [ ( "KeyO", PanSvg.Up )
        , ( "KeyK", PanSvg.Left )
        , ( "KeyL", PanSvg.Down )
        , ( "Semicolon", PanSvg.Right )
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
            case model.mode of
                Capsuling ->
                    rgb 0.2 0.2 0.8

                Downloading ->
                    rgb 0.8 0.8 0.2

                _ ->
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
        [ Input.button []
            { label = text "Ouvrir"
            , onPress = Just ClickedUpload
            }
        , Input.button
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
                (\( s, playing ) ->
                    row [ spacing 5 ]
                        ([ Input.button
                            [ Font.color <|
                                if List.any ((==) s) <| List.map Sound.toString model.loadedSoundList then
                                    rgb 0.2 0.8 0.2

                                else
                                    rgb 1 1 1
                            ]
                            { label = text s
                            , onPress =
                                Just <|
                                    if model.mode == Downloading then
                                        RequestSoundDownload s

                                    else
                                        RequestSoundLoad s
                            }
                         , Input.button []
                            -- Charset ref https://www.w3schools.com/charsets/ref_utf_geometric.asp
                            { label =
                                text <|
                                    if playing then
                                        "◼"

                                    else
                                        "▶"
                            , onPress = Just <| PreListening s <| not playing
                            }
                         ]
                            ++ (if playing then
                                    [ Element.html <|
                                        Html.audio
                                            [ Attr.hidden True
                                            , Attr.src <| Url.Builder.relative [ "sons", s ] []
                                            , Attr.autoplay True
                                            , Events.on "ended" <| D.succeed <| PreListening s False
                                            ]
                                            []
                                    ]

                                else
                                    []
                               )
                        )
                )
             <|
                List.sortWith (\t1 t2 -> Natural.compare (Tuple.first t1) (Tuple.first t2)) <|
                    Dict.toList model.soundList
            )
        ]
    ]


viewLoaded : Model -> List (Element Msg)
viewLoaded model =
    [ column [ width fill, height <| fillPortion 3, spacing 10, padding 2, scrollbarY ] <|
        List.map soundView <|
            List.sortWith
                (\s t -> Natural.compare (Sound.toString s) (Sound.toString t))
                model.loadedSoundList
    ]


soundView : Sound -> Element Msg
soundView s =
    el
        (List.map
            (Element.htmlAttribute
                >> (Element.mapAttribute <| DocMsg << Doc.InteractMsg)
            )
         <|
            Interact.draggableEvents (Interacting.ISound s)
        )
        (text (Sound.toString s))


viewSaveFiles : Model -> List (Element Msg)
viewSaveFiles model =
    [ column [ height <| fillPortion 1, width fill, spacing 20, scrollbarY ]
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
                List.sortWith Natural.compare <|
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
