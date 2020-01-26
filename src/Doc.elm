port module Doc exposing (..)

import Coll exposing (Coll, Id)
import Data exposing (Data)
import Data.Collar as Collar exposing (Colleer)
import Data.Content as Content exposing (Content)
import Data.Mobile as Mobile exposing (Geer, Mobeel)
import Data.Wheel as Wheel exposing (Wheel)
import Editor.Collar as CEditor
import Editor.Common as Editors exposing (Identifier(..))
import Editor.Mobile as MEditor
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Engine
import Html.Attributes
import Interact
import Json.Encode as E
import PanSvg
import Url exposing (Url)


port toEngine : E.Value -> Cmd msg


type alias Doc =
    { data : Data Mobeel
    , viewing : List ( String, Id Geer )
    , editor : MEditor.Model
    }


type Editor
    = M MEditor.Model
    | C CEditor.Model


type alias WContent =
    Content Wheel


init : Maybe Url -> Doc
init url =
    { data = Data.init Mobile.new url
    , viewing = []
    , editor = MEditor.init Nothing Nothing
    }



-- TODO Should be defined in each editor, as Modes


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
    | View (List ( String, Id Geer ))
    | ChangedMode Mode
    | AddContent WContent
    | KeyPressed Shortcut
    | DirectionRepeat PanSvg.Direction
    | MobileMsg MEditor.Msg
    | EditorsMsg Editors.CommonMsg
    | InteractMsg (Interact.Msg Editors.Interactable Editors.Zone)


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
              , editor = MEditor.init Nothing <| Just <| getShared doc
              }
            , toEngine Engine.stop
            )

        Loaded m name ->
            ( { data = Data.load m name doc.data
              , viewing = []
              , editor = MEditor.init (Just m) <| Just <| getShared doc
              }
            , toEngine Engine.stop
            )

        Undo ->
            ( { doc | data = Data.undo doc.data }, Cmd.none )

        Redo ->
            ( { doc | data = Data.redo doc.data }, Cmd.none )

        View l ->
            let
                ( mobile, mayView ) =
                    getViewingCleaned l <| Data.current doc.data
            in
            ( { doc
                | viewing = Maybe.withDefault l mayView
                , editor = MEditor.init (Just mobile) <| Just <| getShared doc
              }
            , toEngine Engine.stop
            )

        ChangedMode mode ->
            case mode of
                MobileMode subMode ->
                    update (MobileMsg <| MEditor.ChangedMode subMode) doc

                CommonMode subMode ->
                    update (MobileMsg <| MEditor.ChangedMode <| MEditor.CommonMode subMode) doc

        AddContent content ->
            update (MobileMsg <| MEditor.NewGear MEditor.defaultAddPos content) doc

        KeyPressed sh ->
            case sh of
                Pack ->
                    update (EditorsMsg <| Editors.TogglePack) doc

                _ ->
                    case sh of
                        Tool i ->
                            case i of
                                1 ->
                                    update (MobileMsg <| MEditor.ChangedTool <| MEditor.Play False False) doc

                                2 ->
                                    update (MobileMsg <| MEditor.ChangedTool <| MEditor.Harmonize) doc

                                3 ->
                                    update (MobileMsg <| MEditor.ChangedTool <| MEditor.Edit) doc

                                _ ->
                                    ( doc, Cmd.none )

                        Play ->
                            update (MobileMsg <| MEditor.ToggleEngine) doc

                        Suppr ->
                            case ( doc.editor.common.edit, doc.editor.tool ) of
                                ( [ G id ], MEditor.Edit ) ->
                                    update (MobileMsg <| MEditor.DeleteGear id) doc

                                _ ->
                                    ( doc, Cmd.none )

                        _ ->
                            ( doc, Cmd.none )

        DirectionRepeat dir ->
            update (MobileMsg <| MEditor.SvgMsg <| PanSvg.Pan dir) doc

        MobileMsg subMsg ->
            let
                mobile =
                    getViewing doc

                res =
                    MEditor.update subMsg ( doc.editor, mobile )

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
                                        Editors.Inside (G id) ->
                                            update
                                                (View <|
                                                    doc.viewing
                                                        ++ [ ( Mobile.gearName id mobile.gears, id ) ]
                                                )
                                                newDoc

                                        _ ->
                                            ( newDoc, Cmd.none )
                                )
                        )
            in
            ( finalDoc
            , Cmd.batch
                [ Cmd.map MobileMsg res.cmd
                , Maybe.withDefault Cmd.none <| Maybe.map toEngine res.toEngine
                , cmd
                ]
            )

        EditorsMsg subMsg ->
            update (MobileMsg <| MEditor.CommonMsg subMsg) doc

        InteractMsg subMsg ->
            update (MobileMsg <| MEditor.InteractMsg subMsg) doc


subs : Doc -> List (Sub Msg)
subs doc =
    List.map (Sub.map MobileMsg) <| MEditor.subs doc.editor


type Mode
    = CommonMode Editors.CommonMode
    | MobileMode MEditor.Mode


keyCodeToMode : List ( String, Mode )
keyCodeToMode =
    List.map (Tuple.mapSecond CommonMode) Editors.keyCodeToMode
        ++ List.map (Tuple.mapSecond MobileMode) MEditor.keyCodeToMode


view : Doc -> Element Msg
view doc =
    let
        ( common, interact ) =
            ( doc.editor.common, doc.editor.interact )
    in
    row [ height fill, width fill ] <|
        (column [ width fill, height fill ]
            ([ viewTop doc
             , el
                [ width fill
                , height fill
                , Element.htmlAttribute <| Html.Attributes.id "svgResizeObserver"
                , Element.inFront <|
                    Editors.viewPack common
                        (List.map (Html.Attributes.map InteractMsg) <|
                            Interact.dragSpaceEvents interact Editors.ZPack
                        )
                        EditorsMsg
                        InteractMsg
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
            ((Element.map MobileMsg <| MEditor.viewTools doc.editor)
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
    [ Element.map MobileMsg <| MEditor.viewExtraTools doc.editor ]


viewSide : Doc -> List (Element Msg)
viewSide doc =
    List.map (Element.map MobileMsg) <| MEditor.viewDetails doc.editor <| getViewing doc


viewContent : Doc -> Element Msg
viewContent doc =
    Element.map MobileMsg <| MEditor.viewContent ( doc.editor, getViewing doc )


getShared : Doc -> ( Editors.CommonModel, PanSvg.Model )
getShared doc =
    ( doc.editor.common, doc.editor.svg )


getViewing : Doc -> Mobeel
getViewing { viewing, data } =
    Tuple.first <| getViewingCleaned viewing <| Data.current data


getViewingCleaned : List ( String, Id Geer ) -> Mobeel -> ( Mobeel, Maybe (List ( String, Id Geer )) )
getViewingCleaned l mobile =
    case l of
        ( str, next ) :: rest ->
            case Coll.maybeGet next mobile.gears of
                Just g ->
                    case Wheel.getContent g of
                        Content.M m ->
                            let
                                ( mob, may ) =
                                    getViewingCleaned rest m
                            in
                            ( mob, Maybe.map ((::) ( str, next )) may )

                        _ ->
                            Debug.log ("No mobile to view in " ++ str) ( mobile, Just [] )

                Nothing ->
                    Debug.log ("No Gear to view at " ++ str) ( mobile, Just [] )

        _ ->
            ( mobile, Nothing )


updateViewing : List ( String, Id Geer ) -> (Mobeel -> Mobeel) -> Mobeel -> Mobeel
updateViewing l f mobile =
    case l of
        ( _, next ) :: rest ->
            case Wheel.getContent <| Coll.get next mobile.gears of
                Content.M subMobile ->
                    { mobile
                        | gears =
                            Coll.update next
                                (Wheel.setContent <| Content.M <| updateViewing rest f subMobile)
                                mobile.gears
                    }

                _ ->
                    Debug.log "IMPOSSIBLE View isn’t correct, should’ve been cleaned" mobile

        _ ->
            f mobile


updateData : Editors.ToUndo -> Mobeel -> Doc -> Data Mobeel
updateData to newMobile { data } =
    case to of
        Editors.Do ->
            Data.do newMobile data

        Editors.Group ->
            Data.group newMobile data

        Editors.Cancel ->
            Data.cancelGroup data

        Editors.NOOP ->
            data
