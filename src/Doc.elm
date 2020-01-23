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
    , viewing : List ( String, Identifier )
    , editor : Editor
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
    , editor = M <| MEditor.init Nothing Nothing
    }



-- TODO Should be defined in each editor, as Modes


type Shortcut
    = Tool Int
    | Play
    | Left
    | Right
    | Suppr


type Msg
    = EnteredFileName String
    | Save
    | Saved
    | New
    | Loaded Mobeel String
    | Undo
    | Redo
    | View (List ( String, Identifier ))
    | ChangedMode Mode
    | AddContent WContent
    | KeyPressed Shortcut
    | DirectionRepeat PanSvg.Direction
    | MobileMsg MEditor.Msg
    | CollarMsg CEditor.Msg
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
            let
                n =
                    init Nothing
            in
            ( { n | data = Data.new Mobile.new doc.data }, toEngine Engine.stop )

        Loaded m name ->
            ( { data = Data.load m name doc.data
              , viewing = []
              , editor = M <| MEditor.init (Just m) <| Just <| getShared doc
              }
            , toEngine Engine.stop
            )

        Undo ->
            ( { doc | data = Data.undo doc.data }, Cmd.none )

        Redo ->
            ( { doc | data = Data.redo doc.data }, Cmd.none )

        View l ->
            let
                mobileContent =
                    Content.M <| Data.current doc.data

                v =
                    cleanViewing l mobileContent
            in
            case getViewingHelper v mobileContent of
                Content.C c ->
                    ( { doc | viewing = v, editor = C <| CEditor.init c <| getShared doc }, toEngine Engine.stop )

                Content.M m ->
                    ( { doc | viewing = l, editor = M <| MEditor.init (Just m) <| Just <| getShared doc }
                    , toEngine Engine.stop
                    )

                _ ->
                    Debug.log "IMPOSSIBLE Cannot view Sound" ( doc, Cmd.none )

        ChangedMode mode ->
            case ( mode, doc.editor ) of
                ( MobileMode subMode, M _ ) ->
                    update (MobileMsg <| MEditor.ChangedMode subMode) doc

                ( CollarMode subMode, C _ ) ->
                    update (CollarMsg <| CEditor.ChangedMode subMode) doc

                ( CommonMode subMode, M _ ) ->
                    update (MobileMsg <| MEditor.ChangedMode <| MEditor.CommonMode subMode) doc

                ( CommonMode subMode, C _ ) ->
                    update (CollarMsg <| CEditor.ChangedMode <| CEditor.CommonMode subMode) doc

                _ ->
                    Debug.log "IMPOSSIBLE Mode for wrong editor" ( doc, Cmd.none )

        AddContent content ->
            case doc.editor of
                M _ ->
                    update (MobileMsg <| MEditor.NewGear MEditor.defaultAddPos content) doc

                C _ ->
                    update (CollarMsg <| CEditor.NewBead content) doc

        KeyPressed sh ->
            case ( sh, doc.editor ) of
                ( Tool i, M _ ) ->
                    case i of
                        1 ->
                            update (MobileMsg <| MEditor.ChangedTool <| MEditor.Play False False) doc

                        2 ->
                            update (MobileMsg <| MEditor.ChangedTool <| MEditor.Harmonize) doc

                        3 ->
                            update (MobileMsg <| MEditor.ChangedTool <| MEditor.Edit) doc

                        _ ->
                            ( doc, Cmd.none )

                ( Tool i, C _ ) ->
                    case i of
                        1 ->
                            update (CollarMsg <| CEditor.ChangedTool <| CEditor.Play False) doc

                        3 ->
                            update (CollarMsg <| CEditor.ChangedTool <| CEditor.Edit) doc

                        _ ->
                            ( doc, Cmd.none )

                ( Play, M _ ) ->
                    update (MobileMsg <| MEditor.ToggleEngine) doc

                ( Play, C _ ) ->
                    update (CollarMsg <| CEditor.ToggleEngine) doc

                ( Left, C _ ) ->
                    update (CollarMsg <| CEditor.CursorLeft) doc

                ( Right, C _ ) ->
                    update (CollarMsg <| CEditor.CursorRight) doc

                ( Suppr, C e ) ->
                    case e.common.edit of
                        [ B i ] ->
                            update (CollarMsg <| CEditor.DeleteBead i) doc

                        _ ->
                            ( doc, Cmd.none )

                ( Suppr, M e ) ->
                    case e.common.edit of
                        [ G id ] ->
                            update (MobileMsg <| MEditor.DeleteGear id) doc

                        _ ->
                            ( doc, Cmd.none )

                _ ->
                    ( doc, Cmd.none )

        DirectionRepeat dir ->
            case doc.editor of
                M editor ->
                    update (MobileMsg <| MEditor.SvgMsg <| PanSvg.Pan dir) doc

                _ ->
                    ( doc, Cmd.none )

        MobileMsg subMsg ->
            case ( doc.editor, getViewing doc ) of
                ( M editor, Content.M mobile ) ->
                    let
                        res =
                            MEditor.update subMsg ( editor, mobile )

                        newMobile =
                            updateMobile doc.viewing (always res.mobile) <| Data.current doc.data

                        data =
                            updateData res.toUndo newMobile doc

                        newDoc =
                            { doc | data = data, editor = M res.model }

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
                                                                ++ [ ( Mobile.gearName id mobile.gears, G id ) ]
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

                _ ->
                    Debug.log "IMPOSSIBLE MobileMsg while viewing no mobile" ( doc, Cmd.none )

        CollarMsg subMsg ->
            case ( doc.editor, getViewing doc ) of
                ( C editor, Content.C collar ) ->
                    let
                        res =
                            CEditor.update subMsg ( editor, collar )

                        newMobile =
                            updateCollar doc.viewing (always res.collar) <| Data.current doc.data

                        data =
                            updateData res.toUndo newMobile doc

                        newDoc =
                            { doc | data = data, editor = C res.model }

                        ( finalDoc, cmd ) =
                            Maybe.withDefault ( newDoc, Cmd.none )
                                (res.outMsg
                                    |> Maybe.map
                                        (\outMsg ->
                                            case outMsg of
                                                Editors.Inside (B id) ->
                                                    update
                                                        (View <|
                                                            doc.viewing
                                                                ++ [ ( Collar.beadName id collar, B id ) ]
                                                        )
                                                        newDoc

                                                _ ->
                                                    ( newDoc, Cmd.none )
                                        )
                                )
                    in
                    ( finalDoc
                    , Cmd.batch
                        [ Cmd.map CollarMsg res.cmd
                        , Maybe.withDefault Cmd.none <| Maybe.map toEngine res.toEngine
                        , cmd
                        ]
                    )

                _ ->
                    Debug.log "IMPOSSIBLE CollarMsg while viewing no collar" ( doc, Cmd.none )

        InteractMsg subMsg ->
            case doc.editor of
                M e ->
                    update (MobileMsg <| MEditor.InteractMsg subMsg) doc

                C e ->
                    update (CollarMsg <| CEditor.InteractMsg subMsg) doc


subs : Doc -> List (Sub Msg)
subs doc =
    case doc.editor of
        M e ->
            List.map (Sub.map MobileMsg) <| MEditor.subs e

        C e ->
            List.map (Sub.map CollarMsg) <| CEditor.subs e


type Mode
    = CommonMode Editors.CommonMode
    | MobileMode MEditor.Mode
    | CollarMode CEditor.Mode


keyCodeToMode : List ( String, Mode )
keyCodeToMode =
    List.map (Tuple.mapSecond CommonMode) Editors.keyCodeToMode
        ++ List.map (Tuple.mapSecond MobileMode) MEditor.keyCodeToMode
        ++ List.map (Tuple.mapSecond CollarMode) CEditor.keyCodeToMode


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
            ((case doc.editor of
                M editor ->
                    Element.map MobileMsg <| MEditor.viewTools editor

                C editor ->
                    Element.map CollarMsg <| CEditor.viewTools editor
             )
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
    case doc.editor of
        M editor ->
            [ Element.map MobileMsg <| MEditor.viewExtraTools editor ]

        _ ->
            []


viewSide : Doc -> List (Element Msg)
viewSide doc =
    case ( doc.editor, getViewing doc ) of
        ( M editor, Content.M m ) ->
            List.map (Element.map MobileMsg) <| MEditor.viewDetails editor m

        ( C editor, Content.C c ) ->
            List.map (Element.map CollarMsg) <| CEditor.viewDetails editor c

        _ ->
            []


viewContent : Doc -> Element Msg
viewContent doc =
    case ( getViewing doc, doc.editor ) of
        ( Content.M m, M editor ) ->
            Element.map MobileMsg <| MEditor.viewContent ( editor, m )

        ( Content.C c, C editor ) ->
            Element.map CollarMsg <| CEditor.viewContent ( editor, c )

        _ ->
            text "Cannot edit Sound currently"


getShared : Doc -> ( Editors.CommonModel, PanSvg.Model )
getShared doc =
    case doc.editor of
        M e ->
            ( e.common, e.svg )

        C e ->
            ( e.common, e.svg )


getViewing : Doc -> WContent
getViewing { viewing, data } =
    getViewingHelper viewing <| Content.M <| Data.current data


getViewingHelper : List ( String, Identifier ) -> WContent -> WContent
getViewingHelper l content =
    case ( l, content ) of
        ( [], _ ) ->
            content

        ( ( _, G next ) :: rest, Content.M m ) ->
            getViewingHelper rest <| Wheel.getContent <| Coll.get next m.gears

        ( ( _, B next ) :: rest, Content.C c ) ->
            getViewingHelper rest <| Wheel.getContent <| Collar.get next c

        _ ->
            Debug.log "IMPOSSIBLE Should’ve cleaned Viewing before get" content



-- TODO Should clean Viewing with each update of data, do it in update before case ?


cleanViewing : List ( String, Identifier ) -> WContent -> List ( String, Identifier )
cleanViewing l content =
    case ( l, content ) of
        ( [], _ ) ->
            []

        ( ( str, G next ) :: rest, Content.M m ) ->
            ( str, G next ) :: (cleanViewing rest <| Wheel.getContent <| Coll.get next m.gears)

        ( ( str, B next ) :: rest, Content.C c ) ->
            ( str, B next ) :: (cleanViewing rest <| Wheel.getContent <| Collar.get next c)

        _ ->
            Debug.log ("Cleaned view " ++ (String.concat <| List.map Tuple.first l) ++ Debug.toString content) []


updateViewing : List ( String, Identifier ) -> (WContent -> WContent) -> WContent -> WContent
updateViewing l f content =
    case ( l, content ) of
        ( [], _ ) ->
            f content

        ( ( _, G next ) :: rest, Content.M m ) ->
            Content.M
                { m
                    | gears =
                        Coll.update next
                            (Wheel.setContent <| updateViewing rest f <| Wheel.getContent <| Coll.get next m.gears)
                            m.gears
                }

        ( ( _, B next ) :: rest, Content.C c ) ->
            Content.C <|
                Collar.updateBead next
                    (Wheel.setContent <| updateViewing rest f <| Wheel.getContent <| Collar.get next c)
                    c

        _ ->
            Debug.log "IMPOSSIBLE Should’ve cleaned Viewing before update" content


updateMobile : List ( String, Identifier ) -> (Mobeel -> Mobeel) -> Mobeel -> Mobeel
updateMobile l f m =
    case
        updateViewing l
            (\c ->
                case c of
                    Content.M mo ->
                        Content.M <| f mo

                    _ ->
                        Debug.log "Can’t update, not viewing mobile" c
            )
        <|
            Content.M m
    of
        Content.M mo ->
            mo

        _ ->
            Debug.log "IMPOSSIBLE Racine isn’t a mobile" m


updateCollar : List ( String, Identifier ) -> (Colleer -> Colleer) -> Mobeel -> Mobeel
updateCollar l f m =
    case
        updateViewing l
            (\c ->
                case c of
                    Content.C co ->
                        Content.C <| f co

                    _ ->
                        Debug.log "Can’t update, not viewing collar" c
            )
        <|
            Content.M m
    of
        Content.M mo ->
            mo

        _ ->
            Debug.log "IMPOSSIBLE Racine isn’t a mobile" m


updateData : Editors.ToUndo -> Mobeel -> Doc -> Data Mobeel
updateData to newMobile { data } =
    case to of
        Editors.Do ->
            Data.do newMobile data

        Editors.Group ->
            Data.group newMobile data

        Editors.NOOP ->
            data
