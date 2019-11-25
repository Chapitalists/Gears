port module Data exposing (..)

import Json.Encode as E
import UndoList as Undo exposing (UndoList)


port saveFile : E.Value -> Cmd msg


type Data data
    = D
        { undoList : UndoList data
        , grouping : Maybe data
        , saved : Bool
        }


init : a -> Data a
init data =
    D
        { undoList = Undo.fresh data
        , grouping = Nothing
        , saved = False
        }


current : Data a -> a
current (D d) =
    case d.grouping of
        Nothing ->
            d.undoList.present

        Just data ->
            data


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


save : Data a -> ( Data a, Cmd msg )
save (D d) =
    ( D { d | saved = True }, saveFile E.null )
