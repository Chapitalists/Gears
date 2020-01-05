port module Doc exposing (..)

import Coll exposing (Coll, Id)
import Collar exposing (Colleer)
import Content exposing (Content)
import Data exposing (Data)
import Editor.Collar as CEditor
import Editor.Mobile as MEditor
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Engine
import Html.Attributes
import Json.Encode as E
import Mobile exposing (Geer, Mobeel)
import PanSvg
import Sound exposing (Sound)
import Url exposing (Url)
import Wheel exposing (Wheel)


port toEngine : E.Value -> Cmd msg


type alias Doc =
    { data : Data Mobeel
    , viewing : List ( String, Identifier )
    , editor : Editor
    }


type Identifier
    = G (Id Geer)
    | B Int


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


type Shortcut
    = Tool Int
    | Play


type Msg
    = EnteredFileName String
    | Save
    | Saved
    | New
    | Loaded Mobeel String
    | Undo
    | Redo
    | View (List ( String, Identifier ))
    | SoundClicked Sound
    | AddContent WContent
    | KeyPressed Shortcut
    | MobileMsg MEditor.Msg
    | CollarMsg CEditor.Msg


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
              , editor = M <| MEditor.init (Just m) <| Just <| getSvg doc
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
                    ( { doc | viewing = v, editor = C <| CEditor.init c <| getSvg doc }, toEngine Engine.stop )

                Content.M m ->
                    ( { doc | viewing = l, editor = M <| MEditor.init (Just m) <| Just <| getSvg doc }
                    , toEngine Engine.stop
                    )

                _ ->
                    Debug.log "IMPOSSIBLE Cannot view Sound" ( doc, Cmd.none )

        SoundClicked sound ->
            case doc.editor of
                M _ ->
                    update (MobileMsg <| MEditor.SoundClicked sound) doc

                C _ ->
                    update (CollarMsg <| CEditor.SoundClicked sound) doc

        AddContent content ->
            case doc.editor of
                M _ ->
                    update (MobileMsg <| MEditor.NewGear content) doc

                C _ ->
                    update (CollarMsg <| CEditor.NewBead content) doc

        KeyPressed sh ->
            case ( sh, doc.editor ) of
                ( Tool i, M _ ) ->
                    case i of
                        1 ->
                            update (MobileMsg <| MEditor.ChangedTool <| MEditor.Play False) doc

                        2 ->
                            update (MobileMsg <| MEditor.ChangedTool <| MEditor.Harmonize) doc

                        3 ->
                            update (MobileMsg <| MEditor.ChangedTool <| MEditor.Edit) doc

                        _ ->
                            ( doc, Cmd.none )

                ( Play, M _ ) ->
                    update (MobileMsg <| MEditor.ToggleEngine) doc

                ( Play, C _ ) ->
                    update (CollarMsg <| CEditor.ToggleEngine) doc

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
                            case res.toUndo of
                                MEditor.Do ->
                                    Data.do newMobile doc.data

                                MEditor.Group ->
                                    Data.group newMobile doc.data

                                MEditor.NOOP ->
                                    doc.data

                        newDoc =
                            { doc | data = data, editor = M res.model }
                    in
                    ( Maybe.withDefault newDoc
                        (res.outMsg
                            |> Maybe.map
                                (\outMsg ->
                                    Tuple.first <|
                                        case outMsg of
                                            MEditor.Inside id ->
                                                update
                                                    (View <|
                                                        doc.viewing
                                                            ++ [ ( Mobile.gearName id mobile.gears, G id ) ]
                                                    )
                                                    newDoc
                                )
                        )
                    , Cmd.batch
                        [ Cmd.map MobileMsg res.cmd
                        , Maybe.withDefault Cmd.none <| Maybe.map toEngine res.toEngine
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
                            case res.toUndo of
                                CEditor.Do ->
                                    Data.do newMobile doc.data

                                CEditor.Group ->
                                    Data.do newMobile doc.data

                                CEditor.NOOP ->
                                    doc.data
                    in
                    ( { doc | data = data, editor = C res.model }
                    , case res.toEngine of
                        Nothing ->
                            Cmd.none

                        Just cmd ->
                            toEngine cmd
                    )

                _ ->
                    Debug.log "IMPOSSIBLE CollarMsg while viewing no collar" ( doc, Cmd.none )


subs : Doc -> List (Sub Msg)
subs doc =
    case doc.editor of
        M e ->
            List.map (Sub.map MobileMsg) <| MEditor.subs e

        C e ->
            List.map (Sub.map CollarMsg) <| CEditor.subs e


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
                    Element.map MobileMsg (MEditor.viewTools editor)

                C editor ->
                    CEditor.viewTools editor
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


getSvg : Doc -> PanSvg.Model
getSvg doc =
    case doc.editor of
        M e ->
            e.svg

        C e ->
            e.svg


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
            ( str, G next ) :: (cleanViewing rest <| Content.M m)

        ( ( str, B next ) :: rest, Content.C c ) ->
            ( str, B next ) :: (cleanViewing rest <| Content.C c)

        _ ->
            Debug.log ("Cleaned view" ++ (String.concat <| List.map Tuple.first l) ++ Debug.toString content) []


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
