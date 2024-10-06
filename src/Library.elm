module Library exposing
    ( Library
    , Msg
    , init
    , libId
    , nearestId
    , update
    , view
    )

import Data.Wheel exposing (Wheel)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Http
import Palette exposing (..)
import Url exposing (Url)


type Library
    = Model Internals


type alias FileList =
    Dict String FileInfo


type alias FileInfo =
    { content : ContentType
    , play : Maybe Float
    , loaded : LoadState
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


type alias Return =
    { model : Internals
    , cmd : Cmd Msg
    , wheel : Maybe Wheel
    }


update : Msg -> Library -> ( Library, Cmd Msg, Maybe Wheel )
update msg (Model model) =
    let
        return : Return
        return =
            { model = model
            , cmd = Cmd.none
            , wheel = Nothing
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
    )
        |> (\ret -> ( Model ret.model, ret.cmd, ret.wheel ))


view : Library -> Float -> Float -> Element Msg
view (Model model) d scale =
    let
        l =
            List.sortBy (fileToMillis << Tuple.second) <| Dict.toList model.files
    in
    column
        --[ scrollbarY
        --, htmlAttribute <| Attr.id libId
        []
    <|
        .l <|
            List.foldr
                (\el acc ->
                    let
                        curD =
                            fileToMillis <| Tuple.second el

                        nearest =
                            curD <= d && acc.lastD > d

                        newLine =
                            viewFile scale nearest el
                    in
                    { lastD = curD, l = newLine :: acc.l }
                )
                { lastD = 1 / 0, l = [] }
                l


viewFile : Float -> Bool -> ( String, FileInfo ) -> Element Msg
viewFile scale nearest ( path, info ) =
    row
        (if nearest then
            [ centerY ]

         else
            []
        )
        --htmlAttribute <| Attr.id <| fileToId ( path, info ) ]
        [ text <| pathToFilename path
        , roundButton
            (round <| scale * fileToMillis info)
            True
            False
            Red
            none
        ]


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


viewLibColumn : List (Element Msg) -> Element Msg
viewLibColumn =
    column
        [ width fill
        , spacing 5
        , padding 2
        , scrollbarY
        , htmlAttribute <| Attr.style "overflow-x" "hidden"
        ]



--TODO Error management


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
                            FileInfo
                                (Sound <| SoundInfo rate samp chan)
                                Nothing
                                Not
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


nearestId : Library -> Float -> String
nearestId (Model model) d =
    case Dict.toList model.files of
        [] ->
            ""

        el :: l ->
            let
                accu =
                    { diff = abs d - fileToMillis (Tuple.second el)
                    , file = el
                    }

                nearestEl =
                    List.foldl
                        (\( path, info ) acc ->
                            let
                                diff =
                                    abs (d - fileToMillis info)
                            in
                            if diff < acc.diff then
                                { diff = diff, file = ( path, info ) }

                            else
                                acc
                        )
                        accu
                        l
            in
            fileToId nearestEl.file


fileToMillis : FileInfo -> Float
fileToMillis { content } =
    case content of
        Sound soundInfo ->
            soundToMillis soundInfo

        Mobile ->
            0

        Collar ->
            0

        Automation ->
            0


soundToMillis : SoundInfo -> Float
soundToMillis { sampleRate, samples } =
    1000 * toFloat samples / toFloat sampleRate


fileToId : ( String, FileInfo ) -> String
fileToId ( str, info ) =
    str ++ "LibEntryId"


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


libId : String
libId =
    "libraryID"
