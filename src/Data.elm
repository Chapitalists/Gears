module Data exposing (..)

import Http
import Json.Encode as E
import UndoList as Undo exposing (UndoList)
import Url exposing (Url)


type Data data
    = D
        { undoList : UndoList data
        , grouping : Maybe data
        , saved : Bool
        , name : String
        , serverUrl : Maybe Url
        }


init : a -> Maybe Url -> Data a
init data url =
    D
        { undoList = Undo.fresh data
        , grouping = Nothing
        , saved = False
        , name = "non-titre"
        , serverUrl = url
        }


new : a -> Data a -> Data a
new data (D d) =
    D
        { d
            | undoList = Undo.new data <| Undo.fresh (current <| D d)
            , grouping = Nothing
            , saved = False
            , name = "non-titre"
        }


load : a -> String -> Data a -> Data a
load data name (D d) =
    D
        { d
            | undoList = Undo.new data <| Undo.fresh (current <| D d)
            , grouping = Nothing
            , saved = True
            , name = name
        }


current : Data a -> a
current (D d) =
    case d.grouping of
        Nothing ->
            d.undoList.present

        Just data ->
            data


isSaved : Data a -> Bool
isSaved (D d) =
    d.saved


getName : Data a -> String
getName (D d) =
    d.name


setName : String -> Data a -> Data a
setName n (D d) =
    D { d | name = n }


do : a -> Data a -> Data a
do data (D d) =
    D { d | saved = False, undoList = Undo.new data d.undoList, grouping = Nothing }


group : a -> Data a -> Data a
group a (D d) =
    D { d | grouping = Just a }


cancelGroup : Data a -> Data a
cancelGroup (D d) =
    D { d | grouping = Nothing }


canUndo : Data a -> Bool
canUndo (D d) =
    Undo.hasPast d.undoList


undo : Data a -> Data a
undo (D d) =
    D { d | saved = False, undoList = Undo.undo d.undoList, grouping = Nothing }


canRedo : Data a -> Bool
canRedo (D d) =
    Undo.hasFuture d.undoList


redo : Data a -> Data a
redo (D d) =
    D { d | saved = False, undoList = Undo.redo d.undoList, grouping = Nothing }


save : Data a -> (a -> E.Value) -> msg -> ( Data a, Cmd msg )
save (D d) encoder cb =
    case d.serverUrl of
        Just url ->
            ( D { d | saved = True }
            , Http.post
                { url = Url.toString { url | path = "/saveFile" }
                , body =
                    Http.jsonBody <|
                        E.object
                            [ ( "name", E.string d.name )
                            , ( "data", encoder d.undoList.present )
                            ]
                , expect = Http.expectWhatever <| always cb -- TODO handle response
                }
            )

        Nothing ->
            -- TODO DL file on client
            ( D d, Cmd.none )
