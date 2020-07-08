port module Doc exposing (..)

import Data exposing (Data)
import Data.Common as Common exposing (Identifier)
import Data.Content as Content exposing (Content)
import Data.Mobile as Mobile exposing (Geer, Mobeel)
import Data.Wheel as Wheel exposing (Conteet, Wheel)
import Editor.Interacting exposing (Interactable, Zone(..))
import Editor.Mobile as Editor
import Element exposing (..)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Engine
import Html.Attributes
import Interact
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Pack exposing (Pack)
import PanSvg
import Url exposing (Url)


port toEngine : E.Value -> Cmd msg


type alias Model =
    { data : Data Doc
    , viewing : List ( String, Identifier )
    , editor : Editor.Model
    , viewComment : Bool
    }


type alias Doc =
    { mobile : Mobeel
    , comment : String
    }


init : Maybe Url -> Model
init url =
    { data = Data.init (Doc Mobile.new "") url
    , viewing = []
    , editor = Editor.init
    , viewComment = False
    }



-- TODO Should be defined in editor, as Modes


type Shortcut
    = Tool Int
    | Play
    | Left
    | Right
    | CleanView
    | Suppr
    | Pack


type Msg
    = EnteredFileName String
    | ChangedComment String
    | ToggleCommentView
    | Save
    | Saved
    | New
    | Loaded Doc String
    | Undo
    | Redo
    | UnFocusComment
    | View (List ( String, Identifier ))
    | AddContent Conteet
    | KeyPressed Shortcut
    | DirectionRepeat PanSvg.Direction
    | MobileMsg Editor.Msg
    | InteractMsg (Interact.Msg Interactable Zone)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg doc =
    -- TODO Maybe clean viewing right here
    case msg of
        EnteredFileName name ->
            if String.all (\c -> Char.isAlphaNum c || c == '-') name then
                ( { doc | data = Data.setName name doc.data }, Cmd.none )

            else
                ( doc, Cmd.none )

        ChangedComment str ->
            let
                oldData =
                    Data.current doc.data
            in
            ( { doc | data = Data.group { oldData | comment = str } doc.data }, Cmd.none )

        ToggleCommentView ->
            ( { doc | viewComment = not doc.viewComment }, Cmd.none )

        Save ->
            let
                ( data, cmd ) =
                    Data.save doc.data encoder Saved
            in
            ( { doc | data = data }, cmd )

        Saved ->
            --TODO handle server response
            ( doc, Cmd.none )

        New ->
            ( { doc
                | data = Data.new (Doc Mobile.new "") doc.data
                , viewing = []
                , editor = Editor.changeView Nothing "" doc.editor
              }
            , toEngine Engine.stop
            )

        Loaded d name ->
            ( { doc
                | data = Data.load d name doc.data
                , viewing = []
                , editor = Editor.changeView (Just d.mobile) "" doc.editor
                , viewComment = doc.viewComment || (not <| String.isEmpty d.comment)
              }
            , toEngine Engine.stop
            )

        Undo ->
            let
                data =
                    Data.undo doc.data

                { mobile, parentUid, cleanedView } =
                    getCleanedView doc
            in
            ( { doc
                | data = data
                , viewing = cleanedView
                , editor = Editor.changeView (Just mobile) parentUid doc.editor
              }
            , toEngine Engine.stop
            )

        Redo ->
            let
                data =
                    Data.redo doc.data

                { mobile, parentUid, cleanedView } =
                    getCleanedView doc
            in
            ( { doc
                | data = data
                , viewing = cleanedView
                , editor = Editor.changeView (Just mobile) parentUid doc.editor
              }
            , toEngine Engine.stop
            )

        UnFocusComment ->
            ( { doc | data = Data.do (Data.current doc.data) doc.data }, Cmd.none )

        View l ->
            let
                { mobile, parentUid, cleanedView } =
                    getCleanedView { doc | viewing = l }
            in
            ( { doc
                | viewing = cleanedView
                , editor = Editor.changeView (Just mobile) parentUid doc.editor
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
                            update (MobileMsg <| Editor.ChangedTool <| Editor.Edit False) doc

                        _ ->
                            ( doc, Cmd.none )

                Play ->
                    case doc.editor.tool of
                        Editor.Play _ _ ->
                            update (MobileMsg <| Editor.ToggleEngine) doc

                        Editor.Edit True ->
                            update (MobileMsg <| Editor.StopGear) doc

                        Editor.Edit False ->
                            update (MobileMsg <| Editor.PlayGear) doc

                        _ ->
                            ( doc, Cmd.none )

                Suppr ->
                    case ( doc.editor.edit, doc.editor.tool ) of
                        ( [ id ], Editor.Edit _ ) ->
                            update (MobileMsg <| Editor.DeleteWheel ( id, [] )) doc

                        _ ->
                            ( doc, Cmd.none )

                Left ->
                    update (MobileMsg <| Editor.CursorLeft) doc

                Right ->
                    update (MobileMsg <| Editor.CursorRight) doc

                CleanView ->
                    let
                        { mobile, parentUid, cleanedView } =
                            getCleanedView doc
                    in
                    ( { doc
                        | viewing = cleanedView
                        , editor = Editor.changeView (Just mobile) parentUid doc.editor
                      }
                    , Cmd.none
                    )

        DirectionRepeat dir ->
            update (MobileMsg <| Editor.SvgMsg <| PanSvg.Pan dir) doc

        MobileMsg subMsg ->
            let
                mobile =
                    getViewing doc

                res =
                    Editor.update subMsg ( doc.editor, mobile )

                newMobile =
                    updateViewing doc.viewing (always res.mobile) (Data.current doc.data).mobile

                data =
                    updateMobileData res.toUndo newMobile doc

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
                                                        (Data.current newDoc.data).mobile
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


subs : Model -> List (Sub Msg)
subs doc =
    List.map (Sub.map MobileMsg) <| Editor.subs doc.editor


keyCodeToMode : List ( String, Editor.Mode )
keyCodeToMode =
    -- FIXME Strange delegation to Editor ?
    Editor.keyCodeToMode


view : Model -> Element Msg
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


viewTop : Model -> Element Msg
viewTop doc =
    column
        ([ width fill ]
            ++ (if doc.viewComment then
                    [ below <| viewComment doc ]

                else
                    []
               )
        )
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
                   , Input.button [ centerX ]
                        { label = text "Undo"
                        , onPress =
                            if Data.canUndo doc.data then
                                Just Undo

                            else
                                Nothing
                        }
                   , Input.button [ centerX ]
                        { label = text "Redo"
                        , onPress =
                            if Data.canRedo doc.data then
                                Just Redo

                            else
                                Nothing
                        }
                   ]
             --++ [ Input.button [ alignRight ]
             --        { label = text "Notes"
             --        , onPress = Just ToggleCommentView
             --        }
             --   ]
            )
        ]


viewNav : Model -> Element Msg
viewNav doc =
    row
        [ padding 10, moveRight 5 ]
    <|
        (List.intersperse (text ">") <|
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
        )


viewBottom : Model -> List (Element Msg)
viewBottom doc =
    [ Element.map MobileMsg <| Editor.viewExtraTools doc.editor ]


viewSide : Model -> List (Element Msg)
viewSide doc =
    List.map (Element.map MobileMsg) <| Editor.viewDetails doc.editor <| getViewing doc


viewContent : Model -> Element Msg
viewContent doc =
    Element.map MobileMsg <| Editor.viewContent ( doc.editor, getViewing doc )


viewComment : Model -> Element Msg
viewComment { data } =
    Input.multiline [ Events.onLoseFocus UnFocusComment ]
        { text = (Data.current data).comment
        , placeholder = Just <| Input.placeholder [] <| text "Laissez ici vos notes ou commentaires"
        , onChange = ChangedComment
        , label = Input.labelHidden "Notes"
        , spellcheck = False
        }


encoder : Doc -> E.Value
encoder d =
    E.object [ ( "mobile", Mobile.encoder d.mobile ), ( "comment", E.string d.comment ) ]


decoder : D.Decoder Doc
decoder =
    Field.attempt "mobile" Mobile.decoder <|
        \mayMobile ->
            case mayMobile of
                Just mobile ->
                    Field.attempt "comment" D.string <|
                        \mayComment ->
                            D.succeed <| Doc mobile <| Maybe.withDefault "" mayComment

                Nothing ->
                    Mobile.decoder |> D.andThen (\m -> D.succeed <| Doc m "")


type alias ViewRes =
    { mobile : Mobeel, parentUid : String, cleanedView : List ( String, Identifier ) }


getViewing : Model -> Mobeel
getViewing doc =
    (getCleanedView doc).mobile


getCleanedView : Model -> ViewRes
getCleanedView { viewing, data } =
    getViewingHelper viewing (Data.current data).mobile



-- TODO Should be able to check id and indexes existence to clean, do it if Common.getWheel? Or make a copy here


getViewingHelper : List ( String, Identifier ) -> Mobeel -> ViewRes
getViewingHelper l mob =
    case l of
        ( str, next ) :: rest ->
            case Wheel.getWheelContent <| Common.getWheel next mob of
                Content.M m ->
                    let
                        { mobile, parentUid, cleanedView } =
                            getViewingHelper rest m
                    in
                    ViewRes mobile (Common.toUid next ++ parentUid) <| ( str, next ) :: cleanedView

                _ ->
                    Debug.log ("No mobile to view in " ++ str) <| ViewRes mob "" []

        _ ->
            ViewRes mob "" []


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


updateMobileData : Editor.ToUndo -> Mobeel -> Model -> Data Doc
updateMobileData to newMobile { data } =
    let
        oldData =
            Data.current data

        newData =
            { oldData | mobile = newMobile }
    in
    case to of
        Editor.Do ->
            Data.do newData data

        Editor.Group ->
            Data.group newData data

        Editor.Cancel ->
            Data.cancelGroup data

        Editor.NOOP ->
            data
