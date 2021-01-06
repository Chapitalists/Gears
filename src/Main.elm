port module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Coll exposing (Coll, Id)
import Data.Collar as Collar
import Data.Content as Content
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


port openMic : () -> Cmd msg


port micOpened : (() -> msg) -> Sub msg


port inputRec : ( String, Int ) -> Cmd msg


port gotNewSample : (D.Value -> msg) -> Sub msg



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
        , subscriptions = subs
        , view = view
        , onUrlRequest = always NOOP
        , onUrlChange = always NOOP
        }



-- MODEL


type alias Model =
    { connected : Bool
    , currentUrl : Url.Url
    , micState : Maybe ( Bool, ( String, Int ) ) -- Recording, FileName, Latency in ms
    , soundList : Dict String SoundListType
    , loadedSoundList : List Sound
    , showDirLoad : Bool
    , savesList : Set String
    , doc : Doc.Model
    , screenSize : ScreenSize
    , fileExplorerTab : ExTab
    , fileFilter : String
    , mode : Mode
    , keys : Keys.State
    }


type SoundListType
    = Playing
    | Stopped
    | Loading
    | Directory Bool (Dict String SoundListType)


type alias ScreenSize =
    { width : Int, height : Int }


type ExTab
    = Sounds
    | LoadedSounds
    | Saves


init : ScreenSize -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init screen url _ =
    ( Model
        False
        url
        Nothing
        Dict.empty
        []
        True
        Set.empty
        (Doc.init <| Just url)
        screen
        Sounds
        ""
        NoMode
        Keys.init
    , Cmd.batch [ fetchSoundList url, fetchSavesList url ]
    )


soundMimeTypes : List String
soundMimeTypes =
    [ "audio/x-wav", "audio/wav" ]



-- UPDATE


type Msg
    = GotSoundList (Result Http.Error String)
    | RequestSoundList
    | PreListening (List String) Bool
    | ExpandDir (List String)
    | RequestSoundLoad (List String)
    | RequestSoundDownload String
    | RequestSavesList
    | RequestSaveLoad String
    | GotSavesList (Result Http.Error String)
    | GotLoadedFile String (Result Http.Error Doc)
    | SoundLoaded (Result D.Error Sound)
    | RequestOpenMic
    | MicOpened
    | StartMicRec
    | EndMicRec String Int
    | EnteredNewRecName String
    | EnteredMicLatency String
    | ClickedUploadSound
    | UploadSounds File (List File)
    | GotNewSample (Result D.Error File)
    | ClickedUploadSave
    | UploadSaves File (List File)
    | ChangedExplorerTab ExTab
    | ToggleShowDirLoad Bool
    | ChgFilter String
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
                            List.foldl
                                (\str dict ->
                                    let
                                        rec subDict list =
                                            case list of
                                                [] ->
                                                    subDict

                                                part :: [] ->
                                                    Dict.insert part Stopped subDict

                                                part :: rest ->
                                                    Dict.update part
                                                        (\mayType ->
                                                            case mayType of
                                                                Just (Directory b nextDict) ->
                                                                    Just <| Directory b <| rec nextDict rest

                                                                Just notDir ->
                                                                    Just notDir

                                                                Nothing ->
                                                                    Just <| Directory False <| rec Dict.empty rest
                                                        )
                                                        subDict
                                    in
                                    rec dict <| List.concatMap (String.split "/") <| String.split "\\" str
                                )
                                Dict.empty
                            <|
                                String.split "\u{0000}" stringList
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
                        | savesList = Set.fromList <| String.split "\\" stringList
                        , connected = True
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | connected = False, savesList = Set.empty }, Cmd.none )

        GotLoadedFile name result ->
            case result of
                Ok doc ->
                    let
                        m =
                            doc.mobile

                        getSoundType el list dict =
                            case ( list, Dict.get el dict ) of
                                ( [], res ) ->
                                    res

                                ( next :: rest, Just (Directory _ d) ) ->
                                    getSoundType next rest d

                                _ ->
                                    Nothing

                        searchReplacement str dict =
                            case Dict.get str dict of
                                Nothing ->
                                    Dict.foldl
                                        (\dir sType res ->
                                            case res of
                                                Just got ->
                                                    Just got

                                                Nothing ->
                                                    case sType of
                                                        Directory _ d ->
                                                            case searchReplacement str d of
                                                                Nothing ->
                                                                    Nothing

                                                                Just sub ->
                                                                    Just (dir :: sub)

                                                        _ ->
                                                            Nothing
                                        )
                                        Nothing
                                        dict

                                Just (Directory _ _) ->
                                    Nothing

                                _ ->
                                    Just [ str ]

                        -- if Loading, do nothing
                        -- if Nothing, search for replacement
                        -- -- if no replacement, abort loading with message no sound found
                        -- -- if replaced, update wheel
                        -- else RequestSoundLoad and update with new soundLib (model)
                        checkLoad content soundLib cmds =
                            case content of
                                Content.S s ->
                                    if List.member s model.loadedSoundList then
                                        Just ( content, soundLib, cmds )

                                    else
                                        let
                                            path =
                                                List.concatMap (String.split "\\") <| String.split "/" <| Sound.toString s
                                        in
                                        case path of
                                            [] ->
                                                Nothing

                                            el :: rest ->
                                                case getSoundType el rest soundLib of
                                                    Just Loading ->
                                                        Just ( content, soundLib, cmds )

                                                    Just Playing ->
                                                        Just
                                                            ( content
                                                            , updateSoundLib soundLib path <| always <| Just Loading
                                                            , (loadSound <| String.join "/" path) :: cmds
                                                            )

                                                    Just Stopped ->
                                                        Just
                                                            ( content
                                                            , updateSoundLib soundLib path <| always <| Just Loading
                                                            , (loadSound <| String.join "/" path) :: cmds
                                                            )

                                                    _ ->
                                                        case List.head <| List.reverse path of
                                                            Just str ->
                                                                case searchReplacement str soundLib of
                                                                    Nothing ->
                                                                        let
                                                                            _ =
                                                                                Debug.log "Cannot find replacement for a sound" str
                                                                        in
                                                                        Nothing

                                                                    Just p ->
                                                                        Just
                                                                            ( Content.S <| Sound.chgPath s <| String.join "/" p
                                                                            , updateSoundLib soundLib p <| always <| Just Loading
                                                                            , (loadSound <| String.join "/" p) :: cmds
                                                                            )

                                                            Nothing ->
                                                                Nothing

                                Content.M mob ->
                                    Maybe.map (\( gears, b, c ) -> ( Content.M { mob | gears = gears }, b, c )) <|
                                        List.foldl
                                            (\( id, g ) mayAcc ->
                                                case mayAcc of
                                                    Just ( coll, sl, cc ) ->
                                                        case checkLoad (Wheel.getContent g) sl cc of
                                                            Just ( newContent, newSL, newCmds ) ->
                                                                Just ( Coll.update id (Wheel.setContent newContent) coll, newSL, newCmds )

                                                            _ ->
                                                                Nothing

                                                    _ ->
                                                        Nothing
                                            )
                                            (Just ( mob.gears, soundLib, cmds ))
                                        <|
                                            Coll.toList mob.gears

                                Content.C col ->
                                    Maybe.map (\( a, b, c ) -> ( Content.C a, b, c )) <|
                                        List.foldl
                                            (\( i, b ) mayAcc ->
                                                case mayAcc of
                                                    Just ( collar, sl, cc ) ->
                                                        case checkLoad (Wheel.getContent b) sl cc of
                                                            Just ( newContent, newSL, newCmds ) ->
                                                                Just ( Content.updateBeadKeepOneSound i (Wheel.setContent newContent) collar, newSL, newCmds )

                                                            _ ->
                                                                Nothing

                                                    _ ->
                                                        Nothing
                                            )
                                            (Just ( col, soundLib, cmds ))
                                        <|
                                            List.indexedMap Tuple.pair <|
                                                Collar.getBeads col

                        {- }
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
                                                           (RequestSoundLoad <|
                                                               List.concatMap (String.split "\\") <|
                                                                   String.split "/" <|
                                                                       Sound.toString s
                                                           )
                                                           model
                                                   ]

                                           Content.M mob ->
                                               loadList <| List.map .wheel <| Coll.values mob.gears

                                           Content.C col ->
                                               loadList <| List.map .wheel <| Collar.getBeads col
                                   )
                                   l
                        -}
                        newModel =
                            { model | connected = True }

                        mayLoad =
                            checkLoad (Content.M m) model.soundList []
                    in
                    case mayLoad of
                        Just ( Content.M newMobile, newSL, cmds ) ->
                            let
                                subMsg =
                                    case newModel.mode of
                                        Capsuling ->
                                            Doc.AddContent <| Content.M newMobile

                                        _ ->
                                            Doc.Loaded { doc | mobile = newMobile } name

                                ( mod, cmd ) =
                                    update (DocMsg subMsg) newModel
                            in
                            ( { mod | soundList = newSL }, Cmd.batch <| cmd :: cmds )

                        _ ->
                            let
                                _ =
                                    Debug.log "Cannot load, sound not found" m
                            in
                            ( model, Cmd.none )

                Err (Http.BadBody err) ->
                    let
                        _ =
                            Debug.log ("Error loading " ++ name ++ " : " ++ err) result
                    in
                    ( model, Cmd.none )

                Err _ ->
                    ( { model | connected = False }, Cmd.none )

        RequestSoundList ->
            ( model, fetchSoundList model.currentUrl )

        RequestSoundLoad id ->
            -- TODO handle no response
            ( { model
                | soundList =
                    updateSoundLib model.soundList id <|
                        \mayType ->
                            case mayType of
                                Nothing ->
                                    Nothing

                                Just (Directory b d) ->
                                    Just (Directory b d)

                                _ ->
                                    Just Loading
              }
            , loadSound <| String.join "/" id
            )

        RequestSoundDownload n ->
            ( model
            , --if Dict.member n model.soundList then
              DL.url <| Url.toString model.currentUrl ++ "sons/" ++ n
              --else
              --  Cmd.none
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
                Err e ->
                    let
                        _ =
                            Debug.log (D.errorToString e) res
                    in
                    ( model, Cmd.none )

                Ok s ->
                    ( { model | loadedSoundList = s :: model.loadedSoundList }, Cmd.none )

        RequestOpenMic ->
            ( model, openMic () )

        MicOpened ->
            ( { model | micState = Just ( False, ( "", 0 ) ) }, Cmd.none )

        StartMicRec ->
            ( { model | micState = Maybe.map (Tuple.mapFirst <| always True) model.micState }
            , inputRec ( "", 0 )
            )

        EndMicRec fileName latency ->
            ( { model | micState = Maybe.map (Tuple.mapFirst <| always False) model.micState }
            , inputRec ( fileName, latency )
            )

        EnteredNewRecName fileName ->
            ( { model | micState = Maybe.map (Tuple.mapSecond <| Tuple.mapFirst <| always fileName) model.micState }
            , Cmd.none
            )

        EnteredMicLatency lat ->
            ( { model
                | micState =
                    Maybe.map
                        (Tuple.mapSecond <|
                            Tuple.mapSecond <|
                                always <|
                                    Maybe.withDefault 0 <|
                                        String.toInt lat
                        )
                        model.micState
              }
            , Cmd.none
            )

        ClickedUploadSound ->
            ( model, Select.files soundMimeTypes UploadSounds )

        UploadSounds f lf ->
            ( model
            , Cmd.batch <|
                List.map
                    (\file ->
                        if List.member (File.mime file) soundMimeTypes && File.size file <= (200 * 1024 * 1024) then
                            Http.post
                                { url = Url.toString model.currentUrl ++ "upSound"
                                , body =
                                    Http.multipartBody
                                        [ Http.filePart "file" file
                                        ]
                                , expect = Http.expectWhatever <| always RequestSoundList
                                }

                        else
                            Cmd.none
                    )
                    (f :: lf)
            )

        GotNewSample res ->
            case res of
                Ok file ->
                    update (UploadSounds file []) model

                Err err ->
                    let
                        _ =
                            Debug.log (D.errorToString err) res
                    in
                    ( model, Cmd.none )

        ClickedUploadSave ->
            ( model, Select.files [] UploadSaves )

        UploadSaves f lf ->
            ( model
            , Cmd.batch <|
                List.map
                    (\file ->
                        if
                            (List.head <| List.reverse <| String.split "." <| File.name file)
                                == Just "gears"
                                && File.size file
                                <= (20 * 1024 * 1024)
                        then
                            Http.post
                                { url = Url.toString model.currentUrl ++ "upSave"
                                , body =
                                    Http.multipartBody
                                        [ Http.filePart "file" file
                                        ]
                                , expect = Http.expectWhatever <| always RequestSavesList
                                }

                        else
                            Cmd.none
                    )
                    (f :: lf)
            )

        PreListening strs p ->
            ( { model
                | soundList =
                    updateSoundLib model.soundList strs <|
                        \mayType ->
                            case mayType of
                                Nothing ->
                                    Nothing

                                Just (Directory b d) ->
                                    Just (Directory b d)

                                Just _ ->
                                    Just <|
                                        if p then
                                            Playing

                                        else
                                            Stopped
              }
            , Cmd.none
            )

        ExpandDir strs ->
            ( { model
                | soundList =
                    updateSoundLib model.soundList strs <|
                        \mayType ->
                            case mayType of
                                Just (Directory b d) ->
                                    Just (Directory (not b) d)

                                _ ->
                                    mayType
              }
            , Cmd.none
            )

        ChangedExplorerTab tab ->
            ( { model | fileExplorerTab = tab }, Cmd.none )

        ToggleShowDirLoad b ->
            ( { model | showDirLoad = b }, Cmd.none )

        ChgFilter s ->
            ( { model | fileFilter = s }, Cmd.none )

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
                    ( { model | doc = doc, fileExplorerTab = LoadedSounds }, Cmd.map DocMsg cmd )

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
        , micOpened <| always MicOpened
        , gotNewSample <| (GotNewSample << D.decodeValue File.decoder)
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
        (row [ Font.size 14, spacing 20 ]
            [ viewExplorerTab model.fileExplorerTab Sounds "Sons"
            , viewExplorerTab model.fileExplorerTab LoadedSounds "Chargés"
            , viewExplorerTab model.fileExplorerTab Saves "Saves"
            ]
            :: Input.text [ Font.color (rgb 0 0 0), paddingXY 5 0 ]
                { label = Input.labelLeft [] <| text "Filtrer\u{202F}:"
                , text = model.fileFilter
                , placeholder = Nothing
                , onChange = ChgFilter
                }
            :: (case model.fileExplorerTab of
                    Sounds ->
                        viewSounds model

                    LoadedSounds ->
                        viewLoaded model

                    Saves ->
                        viewSaveFiles model
               )
        )


viewExplorerTab : ExTab -> ExTab -> String -> Element Msg
viewExplorerTab seled tab name =
    Input.button
        (if seled == tab then
            [ padding 5, Bg.color (rgb 0.2 0.2 0.2) ]

         else
            [ padding 5 ]
        )
        { label = text name
        , onPress = Just <| ChangedExplorerTab tab
        }


viewOpenRefreshButtons : Msg -> Msg -> Bool -> List (Element Msg)
viewOpenRefreshButtons openMsg refreshMsg connected =
    [ Input.button []
        { label = text "Ouvrir"
        , onPress = Just openMsg
        }
    , Input.button
        [ Font.color <|
            if connected then
                rgb 0 0 0

            else
                rgb 1 0 0
        ]
        { onPress = Just refreshMsg
        , label = text "Actualiser"
        }
    ]


viewSounds : Model -> List (Element Msg)
viewSounds model =
    [ column [ width fill, height <| fillPortion 2, spacing 20, scrollbarY ] <|
        [ row [ width fill, spacing 40 ]
            [ column [ spacing 20 ] <|
                viewOpenRefreshButtons ClickedUploadSound RequestSoundList model.connected
            , column [ width fill, spacing 20 ] <|
                case model.micState of
                    Just ( False, ( name, latency ) ) ->
                        [ row []
                            [ Input.button []
                                { onPress =
                                    if String.isEmpty name then
                                        Nothing

                                    else
                                        Just StartMicRec
                                , label = text "Rec Mic"
                                }
                            , Input.text [ Font.color (rgb 0 0 0), paddingXY 5 0 ]
                                { text = String.fromInt latency
                                , placeholder = Nothing
                                , label = Input.labelHidden "Mic latency in ms"
                                , onChange = EnteredMicLatency
                                }
                            ]
                        , Input.text [ Font.color (rgb 0 0 0), paddingXY 5 0 ]
                            { text = name
                            , placeholder = Just <| Input.placeholder [] <| text "Nom du fichier"
                            , label = Input.labelHidden "New File Name"
                            , onChange = EnteredNewRecName
                            }
                        ]

                    Just ( True, ( name, latency ) ) ->
                        [ Input.button [] { onPress = Just <| EndMicRec name latency, label = text "Stop Mic" }
                        , text name
                        ]

                    Nothing ->
                        [ Input.button [] { onPress = Just RequestOpenMic, label = text "Activer Micro" } ]
            ]
        ]
            ++ [ viewLibColumn <| viewLib model [] model.soundList ]
    ]


viewLibColumn : List (Element Msg) -> Element Msg
viewLibColumn =
    column [ width fill, spacing 5, padding 2, scrollbarY ]


viewLib : Model -> List String -> Dict String SoundListType -> List (Element Msg)
viewLib model id dict =
    List.concatMap
        (\( s, sType ) ->
            let
                newId =
                    id ++ [ s ]

                filterOut =
                    not <| String.contains (String.toLower model.fileFilter) <| String.toLower <| String.join "/" newId
            in
            case sType of
                Stopped ->
                    if filterOut then
                        []

                    else
                        [ viewSoundInLib model s newId False False ]

                Playing ->
                    if filterOut then
                        []

                    else
                        [ viewSoundInLib model s newId True False ]

                Loading ->
                    if filterOut then
                        []

                    else
                        [ viewSoundInLib model s newId False True ]

                Directory opened dir ->
                    viewDirInLib model s newId dir opened
        )
    <|
        List.sortWith (\t1 t2 -> Natural.compare (Tuple.first t1) (Tuple.first t2)) <|
            Dict.toList dict


viewSoundInLib : Model -> String -> List String -> Bool -> Bool -> Element Msg
viewSoundInLib model s id playing loading =
    row [ spacing 5 ]
        ([ Input.button
            [ Font.color <|
                if List.any ((==) <| String.join "/" id) <| List.map Sound.toString model.loadedSoundList then
                    rgb 0.2 0.8 0.2

                else if loading then
                    rgb 0.8 0.8 0.2

                else
                    rgb 1 1 1
            ]
            { label = text <| cutExtension s
            , onPress =
                Just <|
                    if model.mode == Downloading then
                        RequestSoundDownload <| String.join "/" id

                    else
                        RequestSoundLoad id
            }
         , Input.button []
            -- Charset ref https://www.w3schools.com/charsets/ref_utf_geometric.asp
            { label =
                text <|
                    if playing then
                        "◼"

                    else
                        "▶"
            , onPress = Just <| PreListening id <| not playing
            }
         ]
            ++ (if playing then
                    [ Element.html <|
                        Html.audio
                            [ Attr.hidden True
                            , Attr.src <| Url.Builder.relative ("sons" :: id) []
                            , Attr.autoplay True
                            , Events.on "ended" <| D.succeed <| PreListening id False
                            ]
                            []
                    ]

                else
                    []
               )
        )


viewDirInLib : Model -> String -> List String -> Dict String SoundListType -> Bool -> List (Element Msg)
viewDirInLib model str id dict opened =
    let
        subView =
            viewLib model id dict
    in
    if not <| List.isEmpty subView then
        Input.button [ Font.color <| rgb 1 1 1 ]
            { label =
                el [ Font.bold ] <|
                    text <|
                        (if opened then
                            "▽"

                         else
                            "◿"
                        )
                            ++ " "
                            ++ str
            , onPress = Just <| ExpandDir id
            }
            :: (if opened then
                    [ el [ moveRight 10 ] <| viewLibColumn subView ]

                else
                    []
               )

    else
        []


viewLoaded : Model -> List (Element Msg)
viewLoaded model =
    [ column [ width fill, height <| fillPortion 3, spacing 10, padding 2, scrollbarY ]
        ([ Input.checkbox []
            { label = Input.labelLeft [] <| text "Voir dossiers"
            , checked = model.showDirLoad
            , onChange = ToggleShowDirLoad
            , icon = Input.defaultCheckbox
            }
         ]
            ++ (List.map (soundView model.showDirLoad) <|
                    List.sortWith (\s t -> Natural.compare (Sound.toString s) (Sound.toString t)) <|
                        filterFiles model.fileFilter Sound.toString model.loadedSoundList
               )
        )
    ]


soundView : Bool -> Sound -> Element Msg
soundView showDir s =
    let
        fullPath =
            cutExtension <| Sound.toString s

        l =
            List.concatMap (String.split "/") <| String.split "\\" fullPath

        justName =
            String.join "/" <| List.drop (List.length l - 1) l
    in
    el
        (List.map
            (Element.htmlAttribute
                >> (Element.mapAttribute <| DocMsg << Doc.InteractMsg)
            )
         <|
            Interact.draggableEvents (Interacting.ISound s)
        )
        (text <|
            if showDir then
                fullPath

            else
                justName
        )


viewSaveFiles : Model -> List (Element Msg)
viewSaveFiles model =
    [ column [ height <| fillPortion 1, width fill, spacing 20, scrollbarY ] <|
        viewOpenRefreshButtons ClickedUploadSave RequestSavesList model.connected
            ++ [ column [ width fill, spacing 5, padding 2, scrollbarY ] <|
                    (List.map (\s -> el [ onClick (RequestSaveLoad s) ] (text <| cutExtension s)) <|
                        List.sortWith Natural.compare <|
                            filterFiles model.fileFilter identity <|
                                Set.toList model.savesList
                    )
               ]
    ]


updateSoundLib :
    Dict String SoundListType
    -> List String
    -> (Maybe SoundListType -> Maybe SoundListType)
    -> Dict String SoundListType
updateSoundLib lib strs up =
    let
        rec dict el list =
            case list of
                [] ->
                    Dict.update el up dict

                next :: rest ->
                    Dict.update el
                        (\mayType ->
                            case mayType of
                                Just (Directory b d) ->
                                    Just (Directory b <| rec d next rest)

                                _ ->
                                    mayType
                        )
                        dict
    in
    case strs of
        [] ->
            lib

        el :: list ->
            rec lib el list



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
        , expect = Http.expectJson (GotLoadedFile <| cutExtension name) Doc.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


filterFiles : String -> (a -> String) -> List a -> List a
filterFiles filter toString =
    List.filter <|
        (String.contains <| String.toLower filter)
            << String.toLower
            << toString


cutExtension : String -> String
cutExtension fullName =
    let
        l =
            String.split "." fullName
    in
    String.join "." <| List.take (List.length l - 1) l
