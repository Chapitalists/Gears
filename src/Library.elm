port module Library exposing
    ( Library
    , Msg
    , init
    , libId
    , update
    , view
    )

import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as D
import Round
import Tools.Utils exposing (htmlId, httpErrorToString)
import Url exposing (Url)


port requestSoundLoading : String -> Cmd msg



--TODO Percents


port gotSoundLoaded : (D.Value -> msg) -> Sub msg


type Library
    = Model Internals


type alias FileList =
    Dict String FileInfo


type alias FileInfo =
    FileItemExtensible {}


type alias FileItem =
    FileItemExtensible { path : String }


type alias FileItemExtensible a =
    { a
        | content : ContentType
        , play : Maybe Float
        , loaded : LoadState
        , millis : Float
    }


type ContentType
    = Sound SoundInfo
    | Mobile
    | Collar
    | Automation


type alias SoundInfo =
    { sampleRate : Int
    , samples : Int
    , channels : Int
    }


type LoadState
    = Not
    | Full
    | Percent Float
    | Failed


type RemoteData data
    = NotAsked
    | Pending
    | NewPending data
    | Got data
    | Error Http.Error


type DirMode
    = Tree (List String) -- opened Dirs
    | Flat Bool -- fullPath


type Filter
    = Content ContentType
    | Duration ( Maybe Float, Maybe Float )
    | Loaded


type alias Internals =
    { serverUrl : Url
    , files : RemoteData FileList
    , dirMode : DirMode
    , filters : List Filter
    , search : String
    }


init : Url -> ( Library, Cmd Msg )
init url =
    ( Model
        { serverUrl = url
        , files = Pending
        , dirMode = Flat False
        , filters = []
        , search = ""
        }
    , Cmd.batch [ fetchSoundList url ]
    )


type Msg
    = RequestFileList
    | GotFileList (Result Http.Error String)
    | FileLoaded (Result D.Error String)
    | ClickFile FileItem


type alias Return =
    { model : Internals
    , cmd : Cmd Msg
    , data : Maybe FileItem
    }


update : Msg -> Library -> ( Library, Cmd Msg, Maybe FileItem )
update msg (Model model) =
    let
        return : Return
        return =
            { model = model
            , cmd = Cmd.none
            , data = Nothing
            }
    in
    (case msg of
        RequestFileList ->
            let
                newModel =
                    case model.files of
                        Got d ->
                            { model | files = NewPending d }

                        _ ->
                            { model | files = Pending }
            in
            { return
                | cmd = fetchSoundList model.serverUrl
                , model = newModel
            }

        GotFileList result ->
            case result of
                Err e ->
                    { return | model = { model | files = Error e } }

                Ok str ->
                    { return
                        | model =
                            { model
                                | files = Got <| manageNewSoundList str
                            }
                    }

        FileLoaded result ->
            let
                dict =
                    failSafeRemoteFiles model.files
                path =
                    case result of
                        Err e ->
                            Debug.log (D.errorToString e) Failed
                        Ok str =

            in

            case result of
                Err e ->

        ClickFile item ->
            let
                dict =
                    failSafeRemoteFiles model.files

                ( newItem, cmd ) =
                    maybeLoad item

                newDict =
                    Dict.insert item.path (itemToInfo newItem) dict
            in
            { return
                | model = { model | files = updateRemoteData model.files newDict }
                , cmd = cmd
                , data = Just newItem
            }
    )
        |> (\ret -> ( Model ret.model, ret.cmd, ret.data ))


sub : Sub Msg
sub =
    gotSoundLoaded (FileLoaded << D.decodeValue (D.field "path" D.string))


view : Library -> Float -> Float -> Element Msg
view (Model model) refD scale =
    case model.files of
        NotAsked ->
            text "Not Asked…"

        Pending ->
            text "Waiting…"

        NewPending data ->
            viewList data refD scale

        Got data ->
            viewList data refD scale

        Error error ->
            text ("Error: " ++ httpErrorToString error)



-- TODO : IDÉE ! pas scrollbar, mais scroll virtuel
-- placer nearest au bon endroit, et le reste "above" ou "below"
-- hover affiche la roue "onRight"


viewList : FileList -> Float -> Float -> Element Msg
viewList dict refD scale =
    let
        proccessedList =
            List.map infoToItem <| Dict.toList dict

        l =
            List.sortBy .millis proccessedList
    in
    case l of
        [] ->
            text "No Files On Server"

        _ ->
            let
                res =
                    List.foldr
                        (\el acc ->
                            let
                                curD =
                                    el.millis

                                isNearest =
                                    curD <= refD && acc.lastD > refD

                                newLine =
                                    viewFile scale refD isNearest el
                            in
                            { lastD = curD
                            , l = newLine :: acc.l
                            , cumulD = acc.cumulD + curD
                            }
                        )
                        { lastD = 1 / 0
                        , l = []
                        , cumulD = 0
                        }
                        l
            in
            column
                [ scrollbarY
                , htmlId libId

                --, moveDown d
                ]
                res.l


viewFile : Float -> Float -> Bool -> FileItem -> Element Msg
viewFile scale refD isNearest item =
    let
        size =
            round <| scale * item.millis
    in
    row
        --[ height <| px size
        ((htmlId <| pathToId item.path)
            :: (if isNearest then
                    [ Font.bold ]

                else
                    []
               )
            ++ [ spacing 10 ]
        )
        [ Input.button []
            { onPress = Just <| ClickFile item
            , label = text <| pathToFilename item.path
            }
        , el [ Font.size 10 ] <|
            text <|
                Round.round 2 (item.millis - refD)
        ]



--viewOpenRefreshButtons : Msg -> Msg -> Bool -> List (Element Msg)
--viewOpenRefreshButtons openMsg refreshMsg connected =
--    [ Input.button []
--        { label = text "Ouvrir"
--        , onPress = Just openMsg
--        }
--    , Input.button
--        [ Font.color <|
--            if connected then
--                rgb 0 0 0
--
--            else
--                rgb 1 0 0
--        ]
--        { onPress = Just refreshMsg
--        , label = text "Actualiser"
--        }
--    ]
--
--
--viewLibColumn : List (Element Msg) -> Element Msg
--viewLibColumn =
--    column
--        [ width fill
--        , spacing 5
--        , padding 2
--        , scrollbarY
--        , htmlAttribute <| Attr.style "overflow-x" "hidden"
--        ]
--TODO Error management
-- TODO Update existing !!!!!!!


manageNewSoundList : String -> FileList
manageNewSoundList str =
    let
        strList =
            String.split "\u{0000}" str

        makeKeyValue strFile =
            let
                pathRecord =
                    String.split "\u{001E}" strFile
            in
            case pathRecord of
                path :: record :: [] ->
                    Maybe.map (\rec -> ( path, rec )) <|
                        makeFileInfo record

                _ ->
                    Nothing

        makeFileInfo strRec =
            let
                record =
                    String.split "\u{001F}" strRec
            in
            case record of
                samples :: channels :: sampleRate :: [] ->
                    Maybe.map3
                        (\samp chan rate ->
                            { content = Sound <| SoundInfo rate samp chan
                            , play = Nothing
                            , loaded = Not
                            , millis = 1000 * toFloat samp / toFloat rate
                            }
                        )
                        (String.toInt samples)
                        (String.toInt channels)
                        (String.toInt sampleRate)

                _ ->
                    Nothing
    in
    Dict.fromList <| List.filterMap makeKeyValue strList



-- HTTP


fetchSoundList : Url.Url -> Cmd Msg
fetchSoundList url =
    Http.get
        { url = Url.toString { url | path = "/soundList" }
        , expect = Http.expectString GotFileList
        }



-- Utils
--nearestId : Library -> Float -> String
--nearestId (Model model) d =
--    case Dict.toList model.files of
--        [] ->
--            ""
--
--        el :: l ->
--            let
--                accu =
--                    { diff = abs d - fileToMillis (Tuple.second el)
--                    , file = el
--                    }
--
--                nearestEl =
--                    List.foldl
--                        (\( path, info ) acc ->
--                            let
--                                diff =
--                                    abs (d - fileToMillis info)
--                            in
--                            if diff < acc.diff then
--                                { diff = diff, file = ( path, info ) }
--
--                            else
--                                acc
--                        )
--                        accu
--                        l
--            in
--            fileToId nearestEl.file
--TODO What about loading again if changed ?!?


maybeLoad : FileItem -> ( FileItem, Cmd msg )
maybeLoad item =
    case item.loaded of
        Not ->
            ( { item | loaded = Percent 0 }
            , requestSoundLoading item.path
            )

        Full ->
            ( item, Cmd.none )

        Percent _ ->
            ( item, Cmd.none )

        Failed ->
            ( { item | loaded = Percent 0 }
            , requestSoundLoading item.path
            )


infoToItem : ( String, FileInfo ) -> FileItem
infoToItem ( path, info ) =
    { path = path
    , content = info.content
    , play = info.play
    , loaded = info.loaded
    , millis = info.millis
    }


itemToInfo : FileItem -> FileInfo
itemToInfo item =
    { content = item.content
    , play = item.play
    , loaded = item.loaded
    , millis = item.millis
    }


fileToId : ( String, FileInfo ) -> String
fileToId ( str, info ) =
    pathToId str


pathToId : String -> String
pathToId str =
    str ++ "LibEntryId"



-- TODO check https://package.elm-lang.org/packages/thomasin/elm-path/


pathToDirList : String -> List String
pathToDirList =
    List.concatMap (String.split "/") << String.split "\\"


pathToFilename : String -> String
pathToFilename path =
    let
        l =
            pathToDirList path
    in
    cutExtension <|
        String.join "" <|
            List.drop (List.length l - 1) l


cutExtension : String -> String
cutExtension fullName =
    let
        l =
            String.split "." fullName
    in
    String.join "." <| List.take (List.length l - 1) l


remoteToMaybe : RemoteData a -> Maybe a
remoteToMaybe remote =
    case remote of
        NotAsked ->
            Nothing

        Pending ->
            Nothing

        NewPending data ->
            Just data

        Got data ->
            Just data

        Error _ ->
            Nothing


updateRemoteData : RemoteData a -> a -> RemoteData a
updateRemoteData remote data =
    case remote of
        NewPending _ ->
            Debug.log "WARNING, new data pending, update’ll be lost…" <|
                NewPending data

        Got _ ->
            Got data

        _ ->
            Debug.log "WARNING, no data, update ignored…" <|
                remote


failSafeRemoteFiles : RemoteData FileList -> FileList
failSafeRemoteFiles =
    Maybe.withDefault Dict.empty << remoteToMaybe


libId : String
libId =
    "libraryID"
