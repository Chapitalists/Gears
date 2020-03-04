port module Doc exposing (..)

import Coll exposing (Coll, Id)
import Data exposing (Data)
import Data.Common as Common exposing (Identifier)
import Data.Content as Content exposing (Content)
import Data.Mobile as Mobile exposing (Geer, Mobeel)
import Data.Wheel as Wheel exposing (Conteet, Wheel)
import Editor.Interacting exposing (Interactable, Zone(..))
import Editor.Mobile as Editor
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Engine
import Html.Attributes
import Interact
import Json.Encode as E
import Pack exposing (Pack)
import PanSvg
import Url exposing (Url)


port toEngine : E.Value -> Cmd msg


type alias Doc =
    { data : Data Mobeel
    , viewing : List ( String, Identifier )
    , editor : Editor.Model
    }


init : Maybe Url -> Doc
init url =
    { data = Data.init Mobile.new url
    , viewing = []
    , editor = Editor.init Nothing Nothing
    }



-- TODO Should be defined in editor, as Modes


type Shortcut
    = Tool Int
    | Play
    | Left
    | Right
    | Suppr
    | Pack


type Msg
    = EnteredFileName String
    | Save
    | Saved
    | New
    | Loaded Mobeel String
    | Undo
    | Redo
    | View (List ( String, Identifier ))
    | AddContent Conteet
    | KeyPressed Shortcut
    | DirectionRepeat PanSvg.Direction
    | MobileMsg Editor.Msg
    | InteractMsg (Interact.Msg Interactable Zone)


update : Msg -> Doc -> ( Doc, Cmd Msg )
update msg doc =
    -- TODO Maybe clean viewing right here
    case msg of
        EnteredFileName name ->
            if String.all (\c -> Char.isAlphaNum c || c == '-') name then
                ( { doc | data = Data.setName name doc.data }, Cmd.none )

            else
                ( doc, Cmd.none )

        Save ->
            let
                ( data, cmd ) =
                    Data.save doc.data Mobile.encoder Saved
            in
            ( { doc | data = data }, cmd )

        Saved ->
            --TODO handle server response
            ( doc, Cmd.none )

        New ->
            ( { data = Data.new Mobile.new doc.data
              , viewing = []
              , editor = Editor.init Nothing <| Just <| Editor.getShared doc.editor
              }
            , toEngine Engine.stop
            )

        Loaded m name ->
            ( { data = Data.load m name doc.data
              , viewing = []
              , editor = Editor.init (Just m) <| Just <| Editor.getShared doc.editor
              }
            , toEngine Engine.stop
            )

        Undo ->
            let
                data =
                    Data.undo doc.data

                mayView =
                    Tuple.second <| getViewingCleaned doc.viewing <| Data.current data
            in
            ( { doc | data = data, viewing = Maybe.withDefault doc.viewing mayView }, toEngine Engine.stop )

        Redo ->
            let
                data =
                    Data.redo doc.data

                mayView =
                    Tuple.second <| getViewingCleaned doc.viewing <| Data.current data
            in
            ( { doc | data = data, viewing = Maybe.withDefault doc.viewing mayView }, toEngine Engine.stop )

        View l ->
            let
                ( mobile, mayView ) =
                    getViewingCleaned l <| Data.current doc.data
            in
            ( { doc
                | viewing = Maybe.withDefault l mayView
                , editor = Editor.init (Just mobile) <| Just <| Editor.getShared doc.editor
              }
            , toEngine Engine.stop
            )

        AddContent content ->
            update (MobileMsg <| Editor.NewGear Editor.defaultAddPos content) doc

        KeyPressed sh ->
            case sh of
                Pack ->
                    update (MobileMsg <| Editor.PackMsg <| Pack.TogglePack) doc

                Tool i ->
                    case i of
                        1 ->
                            update (MobileMsg <| Editor.ChangedTool <| Editor.Play False False) doc

                        2 ->
                            update (MobileMsg <| Editor.ChangedTool <| Editor.Harmonize) doc

                        3 ->
                            update (MobileMsg <| Editor.ChangedTool <| Editor.Edit) doc

                        _ ->
                            ( doc, Cmd.none )

                Play ->
                    update (MobileMsg <| Editor.ToggleEngine) doc

                Suppr ->
                    case ( doc.editor.edit, doc.editor.tool ) of
                        ( [ id ], Editor.Edit ) ->
                            update (MobileMsg <| Editor.DeleteWheel ( id, [] )) doc

                        _ ->
                            ( doc, Cmd.none )

                Left ->
                    update (MobileMsg <| Editor.CursorLeft) doc

                Right ->
                    update (MobileMsg <| Editor.CursorRight) doc

        DirectionRepeat dir ->
            update (MobileMsg <| Editor.SvgMsg <| PanSvg.Pan dir) doc

        MobileMsg subMsg ->
            let
                mobile =
                    getViewing doc

                res =
                    Editor.update subMsg ( doc.editor, mobile )

                newMobile =
                    updateViewing doc.viewing (always res.mobile) <| Data.current doc.data

                data =
                    updateData res.toUndo newMobile doc

                newDoc =
                    { doc | data = data, editor = res.model }

                ( finalDoc, cmd ) =
                    Maybe.withDefault ( newDoc, Cmd.none )
                        (res.outMsg
                            |> Maybe.map
                                (\outMsg ->
                                    case outMsg of
                                        Editor.Inside id ->
                                            update
                                                (View <|
                                                    doc.viewing
                                                        ++ [ ( Common.getName id mobile, id ) ]
                                                )
                                                newDoc

                                        Editor.UnSolo ->
                                            ( newDoc
                                            , Cmd.batch <|
                                                List.map toEngine <|
                                                    Editor.updateAllMuteToEngine newDoc.editor <|
                                                        Data.current newDoc.data
                                            )
                                )
                        )
            in
            ( finalDoc
            , Cmd.batch <|
                [ Cmd.map MobileMsg res.cmd
                , cmd
                ]
                    ++ List.map toEngine res.toEngine
            )

        InteractMsg subMsg ->
            update (MobileMsg <| Editor.InteractMsg subMsg) doc


subs : Doc -> List (Sub Msg)
subs doc =
    List.map (Sub.map MobileMsg) <| Editor.subs doc.editor


keyCodeToMode : List ( String, Editor.Mode )
keyCodeToMode =
    Editor.keyCodeToMode


view : Doc -> Element Msg
view doc =
    row [ height fill, width fill ] <|
        (column [ width fill, height fill ]
            ([ viewTop doc
             , el
                [ width fill
                , height fill
                , Element.htmlAttribute <| Html.Attributes.id "svgResizeObserver"
                ]
               <|
                viewContent doc
             ]
                ++ viewBottom doc
            )
            :: viewSide doc
        )


viewTop : Doc -> Element Msg
viewTop doc =
    column [ width fill ]
        [ viewNav doc
        , row [ width fill, padding 10, spacing 20, Font.size 14 ]
            ((Element.map MobileMsg <| Editor.viewTools doc.editor)
                :: [ Input.text [ width (fill |> maximum 500), centerX ]
                        { label = Input.labelHidden "Nom du fichier"
                        , text = Data.getName doc.data
                        , placeholder = Just <| Input.placeholder [] <| text "nom-a-sauvegarder"
                        , onChange = EnteredFileName
                        }
                   , Input.button
                        [ centerX
                        , Font.color <|
                            if Data.isSaved doc.data then
                                rgb 0 0 0

                            else
                                rgb 0 1 1
                        ]
                        { label = text "Sauvegarder"
                        , onPress = Just Save
                        }
                   , Input.button [ centerX ]
                        { label = text "Nouveau"
                        , onPress = Just New
                        }
                   , Input.button [ alignRight ]
                        { label = text "Undo"
                        , onPress =
                            if Data.canUndo doc.data then
                                Just Undo

                            else
                                Nothing
                        }
                   , Input.button []
                        { label = text "Redo"
                        , onPress =
                            if Data.canRedo doc.data then
                                Just Redo

                            else
                                Nothing
                        }
                   ]
            )
        ]


viewNav : Doc -> Element Msg
viewNav doc =
    row [] <|
        List.intersperse (text ">") <|
            Input.button []
                { label = text "Racine"
                , onPress = Just <| View []
                }
                :: List.indexedMap
                    (\i ( name, _ ) ->
                        Input.button []
                            { label = text name
                            , onPress = Just <| View <| List.take (i + 1) doc.viewing
                            }
                    )
                    doc.viewing


viewBottom : Doc -> List (Element Msg)
viewBottom doc =
    [ Element.map MobileMsg <| Editor.viewExtraTools doc.editor ]


viewSide : Doc -> List (Element Msg)
viewSide doc =
    List.map (Element.map MobileMsg) <| Editor.viewDetails doc.editor <| getViewing doc


viewContent : Doc -> Element Msg
viewContent doc =
    Element.map MobileMsg <| Editor.viewContent ( doc.editor, getViewing doc )


getViewing : Doc -> Mobeel
getViewing { viewing, data } =
    Tuple.first <| getViewingCleaned viewing <| Data.current data



-- TODO Should be able to check id and indexes existence to clean, do it if Common.getWheel? Or make a copy here


getViewingCleaned : List ( String, Identifier ) -> Mobeel -> ( Mobeel, Maybe (List ( String, Identifier )) )
getViewingCleaned l mobile =
    case l of
        ( str, next ) :: rest ->
            case Wheel.getWheelContent <| Common.getWheel next mobile of
                Content.M m ->
                    let
                        ( mob, may ) =
                            getViewingCleaned rest m
                    in
                    ( mob, Maybe.map ((::) ( str, next )) may )

                _ ->
                    Debug.log ("No mobile to view in " ++ str) ( mobile, Just [] )

        _ ->
            ( mobile, Nothing )


updateViewing : List ( String, Identifier ) -> (Mobeel -> Mobeel) -> Mobeel -> Mobeel
updateViewing l f mobile =
    case l of
        ( _, next ) :: rest ->
            case Wheel.getWheelContent <| Common.getWheel next mobile of
                Content.M subMobile ->
                    Common.updateWheel next
                        (Wheel.ChangeContent <| Content.M <| updateViewing rest f subMobile)
                        mobile

                _ ->
                    Debug.log "IMPOSSIBLE View isn’t correct, should’ve been cleaned" mobile

        _ ->
            f mobile


updateData : Editor.ToUndo -> Mobeel -> Doc -> Data Mobeel
updateData to newMobile { data } =
    case to of
        Editor.Do ->
            Data.do newMobile data

        Editor.Group ->
            Data.group newMobile data

        Editor.Cancel ->
            Data.cancelGroup data

        Editor.NOOP ->
            data
