port module Library exposing
    ( Library
    , Msg
    , init
    , sub
    , update
    , viewFileExplorer
    )

import Coll
import Data.Collar as Collar
import Data.Content as Content
import Data.Wheel as Wheel exposing (Wheel)
import Dict exposing (Dict)
import Doc exposing (Doc)
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
import Json.Decode as D
import NaturalOrdering as Natural
import Set exposing (Set)
import Sound exposing (Sound)
import Url exposing (Url)
import Url.Builder


port requestSoundLoading : String -> Cmd msg


port gotSoundLoaded : (D.Value -> msg) -> Sub msg


port gotNewSample : (D.Value -> msg) -> Sub msg


type Library
    = Model Internals


type alias Internals =
    { serverUrl : Url
    , serverResponding : Bool
    , soundList : Dict String SoundListType
    , loadedSoundList : List Sound
    , showDirLoad : Bool
    , savesList : Set String
    , fileExplorerTab : ExTab
    , fileFilter : String
    }


type SoundListType
    = Playing
    | Stopped
    | Loading
    | Directory Bool (Dict String SoundListType)


type ExTab
    = Sounds
    | LoadedSounds
    | Saves


init : Url -> ( Library, Cmd Msg )
init url =
    ( Model
        { serverUrl = url
        , serverResponding = False
        , soundList = Dict.empty
        , loadedSoundList = []
        , showDirLoad = True
        , savesList = Set.empty
        , fileExplorerTab = Sounds
        , fileFilter = ""
        }
    , Cmd.batch [ fetchSoundList url, fetchSavesList url ]
    )


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
    | ClickedUploadSound
    | UploadSounds File (List File)
    | GotNewSample (Result D.Error ( File, SampleType ))
    | ClickedUploadSave
    | UploadSaves File (List File)
    | ChangedExplorerTab ExTab
    | ToggleShowDirLoad Bool
    | ChgFilter String


type alias Return =
    { model : Internals
    , cmd : Cmd Msg
    , wheel : Maybe Wheel
    }


update : Msg -> Library -> ( Library, Cmd Msg, Maybe Wheel )
update msg (Model model) =
    let
        return =
            { model = model
            , cmd = Cmd.none
            , wheel = Nothing
            }
    in
    (case msg of
        GotSoundList result ->
            case result of
                Ok stringList ->
                    { return
                        | model =
                            { model
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
                                , serverResponding = True
                            }
                    }

                Err _ ->
                    { return
                        | model = { model | serverResponding = False }
                    }

        GotSavesList result ->
            case result of
                Ok stringList ->
                    { return
                        | model =
                            { model
                                | savesList = Set.fromList <| String.split "\\" stringList
                                , serverResponding = True
                            }
                    }

                Err _ ->
                    { return
                        | model = { model | serverResponding = False, savesList = Set.empty }
                    }

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

                                                                Just subPath ->
                                                                    Just (dir :: subPath)

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
                                                            , (requestSoundLoading <| String.join "/" path) :: cmds
                                                            )

                                                    Just Stopped ->
                                                        Just
                                                            ( content
                                                            , updateSoundLib soundLib path <| always <| Just Loading
                                                            , (requestSoundLoading <| String.join "/" path) :: cmds
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
                                                                            , (requestSoundLoading <| String.join "/" p) :: cmds
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
                        mayLoad =
                            checkLoad (Content.M m) model.soundList []
                    in
                    case mayLoad of
                        Just ( content, newSL, cmds ) ->
                            { return
                                | model =
                                    { model
                                        | soundList = newSL
                                        , serverResponding = True
                                    }
                                , cmd = Cmd.batch cmds
                                , wheel = Just <| Wheel.fromContent content
                            }

                        _ ->
                            let
                                _ =
                                    Debug.log "Cannot load, sound not found" m
                            in
                            return

                Err (Http.BadBody err) ->
                    let
                        _ =
                            Debug.log ("Error loading " ++ name ++ " : " ++ err) result
                    in
                    return

                Err _ ->
                    { return | model = { model | serverResponding = False } }

        RequestSoundList ->
            { return | cmd = fetchSoundList model.serverUrl }

        RequestSoundLoad id ->
            -- TODO handle no response
            { return
                | model =
                    { model
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
                , cmd = requestSoundLoading <| String.join "/" id
            }

        RequestSoundDownload n ->
            { return
                | cmd =
                    --if Dict.member n model.soundList then
                    DL.url <| Url.toString model.serverUrl ++ "sons/" ++ n

                --else
                --  Cmd.none
            }

        RequestSavesList ->
            { return | cmd = fetchSavesList model.serverUrl }

        RequestSaveLoad n ->
            -- TODO handle no response
            { return
                | cmd =
                    if Set.member n model.savesList then
                        fetchSaveFile model.serverUrl n

                    else
                        Cmd.none
            }

        SoundLoaded res ->
            case res of
                Err e ->
                    let
                        _ =
                            Debug.log (D.errorToString e) res
                    in
                    return

                Ok s ->
                    { return
                        | model =
                            { model | loadedSoundList = s :: model.loadedSoundList }
                    }

        ClickedUploadSound ->
            { return | cmd = Select.files soundMimeTypes UploadSounds }

        UploadSounds f lf ->
            { return
                | cmd =
                    Cmd.batch <|
                        List.map
                            (\file ->
                                if isValidFile file then
                                    Http.post
                                        { url = Url.toString model.serverUrl ++ "upSound"
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
            }

        GotNewSample res ->
            case res of
                Ok ( file, stype ) ->
                    { return
                        | cmd =
                            if isValidFile file then
                                Http.post
                                    { url = Url.toString model.serverUrl ++ "upSound"
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
                    }

                Err err ->
                    let
                        _ =
                            Debug.log (D.errorToString err) res
                    in
                    return

        ClickedUploadSave ->
            { return | cmd = Select.files [] UploadSaves }

        UploadSaves f lf ->
            { return
                | cmd =
                    Cmd.batch <|
                        List.map
                            (\file ->
                                if
                                    (List.head <| List.reverse <| String.split "." <| File.name file)
                                        == Just "gears"
                                        && File.size file
                                        <= (20 * 1024 * 1024)
                                then
                                    Http.post
                                        { url = Url.toString model.serverUrl ++ "upSave"
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
            }

        PreListening strs p ->
            { return
                | model =
                    { model
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
            }

        ExpandDir strs ->
            { return
                | model =
                    { model
                        | soundList =
                            updateSoundLib model.soundList strs <|
                                \mayType ->
                                    case mayType of
                                        Just (Directory b d) ->
                                            Just (Directory (not b) d)

                                        _ ->
                                            mayType
                    }
            }

        ChangedExplorerTab tab ->
            { return | model = { model | fileExplorerTab = tab } }

        ToggleShowDirLoad b ->
            { return | model = { model | showDirLoad = b } }

        ChgFilter s ->
            { return | model = { model | fileFilter = s } }
    )
        |> (\ret -> ( Model ret.model, ret.cmd, ret.wheel ))


sub : Sub Msg
sub =
    [ gotSoundLoaded (SoundLoaded << D.decodeValue Sound.decoder)
    , gotNewSample <| (GotNewSample << D.decodeValue sampleDecoder)
    ]
        |> Sub.batch


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


soundMimeTypes : List String
soundMimeTypes =
    [ "audio/x-wav", "audio/wav" ]


isValidFile : File -> Bool
isValidFile file =
    List.member (File.mime file) soundMimeTypes && File.size file <= (200 * 1024 * 1024)


viewFileExplorer : Library -> Element Msg
viewFileExplorer (Model model) =
    column
        [ height fill
        , Bg.color (rgb 0.5 0.5 0.5)
        , Font.color (rgb 1 1 1)
        , Font.size 16
        , spacing 20
        , padding 10
        ]
    <|
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


viewSounds : Internals -> List (Element Msg)
viewSounds model =
    [ column [ width fill, height <| fillPortion 2, spacing 20, scrollbarY ]
        [ column [ spacing 20 ] <|
            viewOpenRefreshButtons ClickedUploadSound RequestSoundList model.serverResponding
        , viewLibColumn <| viewLib model [] model.soundList
        ]
    ]


viewLibColumn : List (Element Msg) -> Element Msg
viewLibColumn =
    column [ width fill, spacing 5, padding 2, scrollbarY ]


viewLib : Internals -> List String -> Dict String SoundListType -> List (Element Msg)
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


viewSoundInLib : Internals -> String -> List String -> Bool -> Bool -> Element Msg
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
                Just <| RequestSoundLoad id
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


viewDirInLib : Internals -> String -> List String -> Dict String SoundListType -> Bool -> List (Element Msg)
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


viewLoaded : Internals -> List (Element Msg)
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
    el []
        --(List.map
        --    (Element.htmlAttribute
        --        >> (Element.mapAttribute <| DocMsg << Doc.InteractMsg)
        --    )
        -- <|
        --    Interact.draggableEvents (Interacting.ISound s)
        --)
        (text <|
            if showDir then
                fullPath

            else
                justName
        )


viewSaveFiles : Internals -> List (Element Msg)
viewSaveFiles model =
    [ column [ height <| fillPortion 1, width fill, spacing 20, scrollbarY ] <|
        viewOpenRefreshButtons ClickedUploadSave RequestSavesList model.serverResponding
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
