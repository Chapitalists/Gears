port module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Coll exposing (Coll, Id)
import Color
import Data.Collar as Collar
import Data.Content as Content
import Data.Wheel as Wheel
import Dict exposing (Dict)
import Doc exposing (Doc)
import Editor.Interacting as Interacting
import Editor.Mobile as Editor
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
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
import Panel exposing (Panel)
import Result exposing (Result)
import Set exposing (Set)
import Sound exposing (Sound)
import Url exposing (Url)
import Url.Builder


port loadSound : String -> Cmd msg


port soundLoaded : (D.Value -> msg) -> Sub msg


port requestDeviceList : () -> Cmd msg


port gotDeviceList : (D.Value -> msg) -> Sub msg


port changeSink : String -> Cmd msg


port gotMaxChannel : (Int -> msg) -> Sub msg


port openMic : () -> Cmd msg


port micOpened : (() -> msg) -> Sub msg


port inputRec : ( String, Bool ) -> Cmd msg


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
    , hasSinkId : Bool
    , deviceList : Maybe (List SoundDevice)
    , micState : Maybe ( Bool, String ) -- Recording, FileName
    , soundList : Dict String SoundListType
    , loadedSoundList : List Sound
    , showDirLoad : Bool
    , savesList : Set String
    , doc : ( Doc.Model, Maybe Int ) -- maxChannelCount if real channels
    , screenSize : ScreenSize
    , fileExplorerTab : ExTab
    , fileFilter : String
    , libPanel : Panel
    , mode : Mode
    , keys : Keys.State
    , creditsOpened : Bool
    }


type SoundListType
    = Playing
    | Stopped
    | Loading
    | Directory Bool (Dict String SoundListType)


type alias SoundDevice =
    { label : String, deviceId : String }


soundDeviceDecoder : D.Decoder SoundDevice
soundDeviceDecoder =
    D.map2 SoundDevice
        (D.field "label" D.string)
        (D.field "deviceId" D.string)


setMaxChannel : Int -> Model -> Model
setMaxChannel i m =
    { m | doc = ( Tuple.first m.doc, Just i ) }


rmMaxChannel : Model -> Model
rmMaxChannel m =
    { m | doc = ( Tuple.first m.doc, Nothing ) }


type alias ScreenSize =
    { width : Int, height : Int }


type ExTab
    = Sounds
    | LoadedSounds
    | Saves


type alias Flags =
    { hasSinkId : Bool, screen : ScreenSize }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { hasSinkId, screen } url _ =
    ( Model
        False
        url
        hasSinkId
        Nothing
        Nothing
        Dict.empty
        []
        True
        Set.empty
        ( Doc.init <| Just url, Nothing )
        screen
        Sounds
        ""
        (Panel Panel.Shown Panel.Left 200)
        NoMode
        Keys.init
        False
    , Cmd.batch [ fetchSoundList url, fetchSavesList url ]
    )


soundMimeTypes : List String
soundMimeTypes =
    [ "audio/x-wav", "audio/wav" ]


isValidFile : File -> Bool
isValidFile file =
    List.member (File.mime file) soundMimeTypes && File.size file <= (200 * 1024 * 1024)



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
    | RequestDeviceList
    | GotDeviceList (Result D.Error (List SoundDevice))
    | ChangeSink String
    | VirtualChannels
    | GotMaxChannel Int
    | RequestOpenMic
    | MicOpened
    | StartMicRec
    | EndMicRec String
    | EnteredNewRecName String
    | ClickedUploadSound
    | UploadSounds File (List File)
    | GotNewSample (Result D.Error ( File, SampleType ))
    | ClickedUploadSave
    | UploadSaves File (List File)
    | ChangedExplorerTab ExTab
    | ToggleShowDirLoad Bool
    | ChgFilter String
    | ChangedMode Mode
    | GotScreenSize ScreenSize
    | PanelMsg Panel.Msg
    | DocMsg Doc.Msg
    | KeysMsg Keys.Msg
    | Credits Bool
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
                                Content.None ->
                                    Just ( content, soundLib, cmds )

                                Content.S s ->
                                    if List.member s model.loadedSoundList then
                                        Just ( content, soundLib, cmds )

                                    else
                                        let
                                            path =
                                                List.concatMap (String.split "\\") <| String.split "/" <| Sound.getPath s
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

        RequestDeviceList ->
            ( model, requestDeviceList () )

        GotDeviceList res ->
            case res of
                Ok dl ->
                    ( { model | deviceList = Just dl }, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log (D.errorToString error) res
                    in
                    ( model, Cmd.none )

        ChangeSink int ->
            ( model, changeSink int )

        GotMaxChannel c ->
            ( setMaxChannel c model, Cmd.none )

        VirtualChannels ->
            ( rmMaxChannel { model | deviceList = Nothing }, Cmd.none )

        RequestOpenMic ->
            ( model, openMic () )

        MicOpened ->
            ( { model | micState = Just ( False, "" ) }, Cmd.none )

        StartMicRec ->
            ( { model | micState = Maybe.map (Tuple.mapFirst <| always True) model.micState }
            , inputRec ( "", Coll.isEmpty (Doc.getViewing <| Tuple.first model.doc).gears )
            )

        EndMicRec fileName ->
            ( { model | micState = Maybe.map (Tuple.mapFirst <| always False) model.micState }
            , inputRec ( fileName, True )
            )

        EnteredNewRecName fileName ->
            ( { model | micState = Maybe.map (Tuple.mapSecond <| always fileName) model.micState }
            , Cmd.none
            )

        ClickedUploadSound ->
            ( model, Select.files soundMimeTypes UploadSounds )

        UploadSounds f lf ->
            ( model
            , Cmd.batch <|
                List.map
                    (\file ->
                        if isValidFile file then
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
                Ok ( file, stype ) ->
                    ( model
                    , if isValidFile file then
                        Http.post
                            { url = Url.toString model.currentUrl ++ "upSound"
                            , body =
                                Http.multipartBody <|
                                    Http.filePart "file" file
                                        :: (case stype of
                                                Reced ->
                                                    [ Http.stringPart "type" "REC" ]

                                                Cuted from ->
                                                    [ Http.stringPart "type" "CUT"
                                                    , Http.stringPart "from" from
                                                    ]
                                           )
                            , expect = Http.expectWhatever <| always RequestSoundList
                            }

                      else
                        Cmd.none
                    )

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

        PanelMsg subMsg ->
            ( { model | libPanel = Panel.update subMsg model.libPanel }, Cmd.none )

        DocMsg subMsg ->
            let
                ( doc, chans ) =
                    model.doc

                ( newModel, cmd ) =
                    case ( model.micState, subMsg, doc.editor.tool ) of
                        -- FIXME Absurd... Should be a commonMsg and common ChangedMode
                        ( _, Doc.MobileMsg (Editor.ChangedMode (Editor.ChangeSound _)), _ ) ->
                            ( { model | fileExplorerTab = LoadedSounds }, Cmd.none )

                        ( Just ( True, name ), Doc.MobileMsg Editor.ToggleEngine, Editor.Play True _ ) ->
                            update (EndMicRec name) model

                        _ ->
                            ( model, Cmd.none )

                ( newDoc, subCmd ) =
                    Doc.update subMsg ( doc, chans )
            in
            ( { newModel | doc = ( newDoc, chans ) }, Cmd.batch <| cmd :: [ Cmd.map DocMsg subCmd ] )

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
                            case Dict.get code <| keyCodeToShortcut model of
                                Just press ->
                                    let
                                        ( doc, chans ) =
                                            model.doc

                                        ( newModel, cmd ) =
                                            case ( model.micState, press, doc.editor.tool ) of
                                                ( Just ( True, name ), Doc.Play, Editor.Play True _ ) ->
                                                    update (EndMicRec name) model

                                                _ ->
                                                    ( m, Cmd.none )

                                        ( newDoc, docCmd ) =
                                            Doc.update (Doc.KeyPressed press) newModel.doc
                                    in
                                    ( { newModel | doc = ( newDoc, chans ) }, Cmd.batch [ c, Cmd.map DocMsg docCmd, cmd ] )

                                Nothing ->
                                    ( m, c )

                        Keys.Repeat code ->
                            case Dict.get code keyCodeToDirection of
                                Just dirMsg ->
                                    let
                                        ( dirModel, cmd ) =
                                            update dirMsg m
                                    in
                                    ( dirModel, Cmd.batch [ c, cmd ] )

                                Nothing ->
                                    ( m, c )
                )
                ( { model | keys = state }, Cmd.none )
                events

        Credits open ->
            ( { model | creditsOpened = open }, Cmd.none )

        NOOP ->
            ( model, Cmd.none )



-- SUBS


subs : Model -> Sub Msg
subs { doc } =
    Sub.batch <|
        [ soundLoaded (SoundLoaded << D.decodeValue Sound.decoder)
        , BE.onResize (\w h -> GotScreenSize { width = w, height = h })
        , gotDeviceList <| GotDeviceList << D.decodeValue (D.list soundDeviceDecoder)
        , gotMaxChannel GotMaxChannel
        , micOpened <| always MicOpened
        , gotNewSample <| (GotNewSample << D.decodeValue sampleDecoder)
        ]
            ++ List.map (Sub.map DocMsg) (Doc.subs <| Tuple.first doc)
            ++ List.map (Sub.map KeysMsg) Keys.subs


type SampleType
    = Reced
    | Cuted String


sampleDecoder : D.Decoder ( File, SampleType )
sampleDecoder =
    D.map2 Tuple.pair (D.field "file" File.decoder) <|
        D.andThen
            (\s ->
                case s of
                    "rec" ->
                        D.succeed Reced

                    "cut" ->
                        D.map (Cuted << Sound.fileNameFromPath) <| D.field "from" D.string

                    _ ->
                        D.fail "not valid type to decode sample"
            )
        <|
            D.field "type" D.string


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


keyCodeToShortcut : Model -> Dict String Doc.Shortcut
keyCodeToShortcut model =
    Dict.union
        (Dict.fromList
            [ ( "KeyZ", Doc.Tool 1 )
            , ( "KeyX", Doc.Tool 2 )
            , ( "KeyC", Doc.Tool 3 )
            , ( "Space", Doc.Play )
            , ( "ArrowLeft", Doc.Left )
            , ( "ArrowRight", Doc.Right )
            , ( "KeyT", Doc.Pack )
            ]
        )
    <|
        Doc.keyCodeToShortcut <|
            Tuple.first model.doc


keyCodeToDirection : Dict String Msg
keyCodeToDirection =
    Dict.union
        (Dict.map (always <| DocMsg << Doc.DirectionRepeat) <|
            Dict.fromList
                [ ( "KeyO", PanSvg.Up )
                , ( "KeyK", PanSvg.Left )
                , ( "KeyL", PanSvg.Down )
                , ( "Semicolon", PanSvg.Right )
                ]
        )
    <|
        Dict.map (always DocMsg) Doc.keyCodeToDirection



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        creditsView =
            if model.creditsOpened then
                viewCredits

            else
                Input.button
                    [ alignBottom
                    , alignRight
                    , Font.size 12
                    , Font.underline
                    , padding 6
                    ]
                    { onPress = Just <| Credits True
                    , label = text "Credits"
                    }
    in
    { title =
        "Gears !"
            ++ (if model.connected then
                    ""

                else
                    " - DISCONNECTED"
               )
    , body =
        [ layout [ inFront creditsView ] <|
            -- Don’t know why but it doesn’t work like I want when using el
            row
                [ height <| px model.screenSize.height
                , width <| px model.screenSize.width
                , Panel.view PanelMsg model.screenSize model.libPanel <|
                    viewFileExplorer model
                ]
                [ Element.map DocMsg <| Doc.view model.doc ]
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
    column
        [ height fill
        , Bg.color bgColor
        , Font.color (rgb 1 1 1)
        , Font.size 16
        , spacing 20
        , padding 10
        ]
    <|
        (roundButton 40 False Color.lightPurple (tabImage Sounds 40 40 "")
            :: roundButton 100 True Color.lightOrange (tabImage Saves 100 100 "")
            :: Input.radioRow
                [ moveUp 1
                , spacing 10
                , Border.widthEach
                    { bottom = 3
                    , top = 0
                    , left = 0
                    , right = 0
                    }
                , Border.color
                    (rgb 0.8 0.5 0.2)
                ]
                { onChange = ChangedExplorerTab
                , selected = Just model.fileExplorerTab
                , label = Input.labelHidden "Explorer Tabs"
                , options =
                    [ viewExplorerOption Sounds
                    , viewExplorerOption LoadedSounds
                    , viewExplorerOption Saves
                    ]
                }
            :: (if model.hasSinkId then
                    viewSinkSelect model

                else
                    none
               )
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


viewExplorerOption : ExTab -> Input.Option ExTab Msg
viewExplorerOption tab =
    let
        size =
            40
    in
    Input.optionWith tab <|
        \state ->
            roundButton size (state == Input.Selected) Color.lightBlue <|
                tabImage tab size size <|
                    case tab of
                        Sounds ->
                            "Sons LOCALIZE"

                        LoadedSounds ->
                            "Chargés LOCALIZE"

                        Saves ->
                            "Saves LOCALIZE"


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


viewSinkSelect : Model -> Element Msg
viewSinkSelect { deviceList } =
    case deviceList of
        Nothing ->
            Input.button []
                { label = text "Lister Sorties"
                , onPress = Just RequestDeviceList
                }

        Just devList ->
            row [ padding 2, width shrink ]
                [ html <|
                    Html.select [ Events.onInput ChangeSink ] <|
                        List.map
                            (\{ label, deviceId } ->
                                Html.option
                                    ([ Attr.value deviceId ]
                                        ++ (if deviceId == "default" then
                                                [ Attr.selected True ]
                                                --TODO doesnt seem to work

                                            else
                                                []
                                           )
                                    )
                                    [ Html.text label ]
                            )
                            devList
                , Input.button []
                    { label = text "X"
                    , onPress = Just VirtualChannels
                    }
                ]


viewSounds : Model -> List (Element Msg)
viewSounds model =
    [ column [ width fill, height <| fillPortion 2, spacing 20, scrollbarY ] <|
        [ row [ width fill, spacing 40 ]
            [ column [ spacing 20 ] <|
                viewOpenRefreshButtons ClickedUploadSound RequestSoundList model.connected
            , viewMicRec model
            ]
        ]
            ++ [ viewLibColumn <| viewLib model [] model.soundList ]
    ]


viewMicRec : Model -> Element Msg
viewMicRec model =
    column [ width fill, spacing 20 ] <|
        case model.micState of
            Just ( False, name ) ->
                [ row []
                    [ Input.button []
                        { onPress =
                            if String.isEmpty name then
                                Nothing

                            else
                                Just StartMicRec
                        , label = text "Rec Mic"
                        }
                    ]
                , Input.text [ Font.color (rgb 0 0 0), paddingXY 5 0 ]
                    { text = name
                    , placeholder = Just <| Input.placeholder [] <| text "Nom du fichier"
                    , label = Input.labelHidden "New File Name"
                    , onChange = EnteredNewRecName
                    }
                ]

            Just ( True, name ) ->
                [ Input.button [] { onPress = Just <| EndMicRec name, label = text "Stop Mic" }
                , text name
                ]

            Nothing ->
                [ Input.button [] { onPress = Just RequestOpenMic, label = text "Activer Micro" } ]


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
                if List.any ((==) <| String.join "/" id) <| List.map Sound.getPath model.loadedSoundList then
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
                    List.sortWith (\s t -> Natural.compare (Sound.getPath s) (Sound.getPath t)) <|
                        filterFiles model.fileFilter Sound.getPath model.loadedSoundList
               )
        )
    ]


soundView : Bool -> Sound -> Element Msg
soundView showDir s =
    let
        fullPath =
            cutExtension <| Sound.getPath s

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


viewCredits : Element Msg
viewCredits =
    el [ padding 40, width fill, height fill ] <|
        el
            [ width fill
            , height fill
            , Bg.color (rgba 0.6 0.6 0.6 0.9)
            , Border.width 2
            , Border.rounded 10
            , inFront <|
                el
                    [ alignTop
                    , alignRight
                    , moveDown 20
                    , moveLeft 20
                    ]
                <|
                    roundButton 40 False Color.lightRed <|
                        Input.button
                            [ Font.size 40
                            , moveUp 4
                            , moveLeft 1
                            , Font.center
                            ]
                            { onPress = Just <| Credits False
                            , label = text "🗙"
                            }
            ]
        <|
            row [ centerX, centerY ]
                (credits
                    |> List.map
                        (\( str, url ) ->
                            newTabLink
                                [ Font.underline ]
                                { url = url, label = text str }
                        )
                )


credits : List ( String, String )
credits =
    [ ( "Book by Made by Made from Noun Project (CC BY 3.0)"
      , "https://thenounproject.com/browse/icons/term/book/"
      )
    ]


tabImage : ExTab -> Int -> Int -> String -> Element msg
tabImage tab x y desc =
    el
        [ width <| px x
        , height <| px y
        , clip
        ]
    <|
        image []
            { description = desc -- TODO localize
            , src =
                case tab of
                    Sounds ->
                        "./icons/bookshelf-noun-books-1336202.svg"

                    LoadedSounds ->
                        "./icons/bookopened-noun-book-1360734.svg"

                    Saves ->
                        "./icons/bookfav-noun-books-1368335.svg"
            }


roundButton : Int -> Bool -> Color.Color -> Element msg -> Element msg
roundButton sizeEl seled bg =
    let
        sizeIn =
            ceiling <| toFloat sizeEl * sqrt 2.0

        stroke =
            ceiling <|
                toFloat sizeIn
                    / 30

        size =
            sizeIn + stroke * 2

        sizeOut =
            size + stroke * 2

        selFactor =
            if seled then
                1

            else
                0
    in
    el
        [ Border.width 1
        , Border.color (rgba 0 0 0 0)
        , mouseDown [ Border.color (rgb 0 0 0) ]
        , Border.rounded sizeOut
        ]
        << el
            [ Border.color (rgba 0.8 0.5 0.2 selFactor)
            , paddingEach
                { bottom = stroke * selFactor
                , left = 0
                , right = 0
                , top = 0
                }
            , Border.widthEach
                { bottom = stroke * (1 - selFactor)
                , left = stroke
                , right = stroke
                , top = stroke
                }
            , Border.roundEach
                { topLeft = size
                , topRight = size
                , bottomLeft = size * (1 - selFactor)
                , bottomRight = size * (1 - selFactor)
                }
            , mouseDown [ Border.color (rgba 0.8 0.5 0.2 selFactor) ]
            , mouseOver [ Border.color (rgb (0.8 * selFactor) (0.5 * selFactor) (0.2 * selFactor)) ]
            ]
        << el
            [ Bg.color <| colToEl bg
            , width <| px sizeIn
            , height <| px sizeIn
            , Border.color (rgb 0 0 0)
            , Border.width stroke
            , Border.rounded sizeIn
            , clip
            , padding <| (sizeIn - sizeEl) // 2
            ]


colToEl : Color.Color -> Color
colToEl =
    fromRgb << Color.toRgba
