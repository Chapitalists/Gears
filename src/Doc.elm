port module Doc exposing (..)

import Coll exposing (Coll, Id)
import Collar exposing (Colleer)
import Color
import Content exposing (Content)
import Data exposing (Data)
import Editor.Collar as CEditor
import Editor.Mobile as MEditor
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Engine
import Harmony as Harmo
import Interact
import Json.Encode as E
import Math.Vector2 exposing (Vec2, vec2)
import Mobile exposing (Geer, Mobeel)
import Random
import Sound exposing (Sound)
import TypedSvg.Core as Svg
import Url exposing (Url)
import Wheel exposing (Wheel)


port toEngine : E.Value -> Cmd msg


type Doc
    = D
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
    D
        { data = Data.init Mobile.new url
        , viewing = []
        , editor = M MEditor.init
        }



-- TODO why not msg in update ?


changeMobile : Mobeel -> String -> Doc -> Doc
changeMobile m name (D d) =
    let
        (D n) =
            init Nothing
    in
    D { n | data = Data.load m name d.data }



-- TODO Should be in Mobile.update, but shouldn’t return pos for that
-- Possible when viewPos moves to Mobile


soundClicked : Sound -> Doc -> ( Doc, Maybe Vec2, Cmd Msg )
soundClicked sound (D doc) =
    case doc.editor of
        C editor ->
            ( D
                { doc
                    | data =
                        Data.do
                            (updateCollar doc.viewing
                                (Collar.add editor.cursor <| Collar.beadFromSound sound)
                                (Data.current doc.data)
                            )
                            doc.data
                }
            , Nothing
            , Cmd.none
            )

        M editor ->
            case ( editor.mode, getViewing (D doc) ) of
                ( MEditor.ChangeSound id, Content.M mobile ) ->
                    let
                        group =
                            Harmo.getHarmonicGroup (Coll.idMap id) mobile.gears

                        chSound =
                            Wheel.update <| Wheel.ChangeContent <| Content.S sound
                    in
                    ( D
                        { doc
                            | data =
                                Data.do
                                    (updateMobile doc.viewing
                                        (\m ->
                                            { m
                                                | gears =
                                                    List.foldl (\el -> Coll.update el chSound) m.gears group
                                            }
                                        )
                                     <|
                                        Data.current doc.data
                                    )
                                    doc.data
                            , editor = M { editor | mode = MEditor.Normal }
                        }
                    , Nothing
                    , Cmd.none
                    )

                ( _, Content.M mobile ) ->
                    let
                        pos =
                            vec2 50 50

                        ( id, gears ) =
                            Coll.insertTellId (Mobile.gearFromSound sound pos) mobile.gears

                        colorGen =
                            Random.map (\f -> Color.hsl f 1 0.5) <| Random.float 0 1
                    in
                    ( D
                        { doc
                            | data =
                                Data.do
                                    (updateMobile doc.viewing
                                        (\m ->
                                            { m | gears = gears }
                                        )
                                     <|
                                        Data.current doc.data
                                    )
                                    doc.data
                        }
                    , Just pos
                    , Random.generate (\c -> MobileMsg <| MEditor.WheelMsg ( id, Wheel.ChangeColor c )) colorGen
                    )

                _ ->
                    ( D doc, Nothing, Cmd.none )


type Shortcut
    = Tool Int
    | Play
    | Nav
    | Normal


type Msg
    = EnteredFileName String
    | Save
    | Saved
    | New
    | Undo
    | Redo
    | View (List ( String, Identifier ))
    | KeyPressed Shortcut
    | MobileMsg MEditor.Msg
    | CollarMsg CEditor.Msg
    | InteractEvent (Interact.Event MEditor.Interactable)


update : Msg -> Float -> Doc -> ( Doc, Cmd Msg )
update msg scale (D doc) =
    -- TODO Maybe clean view right here
    case msg of
        EnteredFileName name ->
            if String.all (\c -> Char.isAlphaNum c || c == '-') name then
                ( D { doc | data = Data.setName name doc.data }, Cmd.none )

            else
                ( D doc, Cmd.none )

        Save ->
            let
                ( data, cmd ) =
                    Data.save doc.data Mobile.encoder Saved
            in
            ( D { doc | data = data }, cmd )

        Saved ->
            --TODO handle server response
            ( D doc, Cmd.none )

        New ->
            let
                (D n) =
                    init Nothing
            in
            ( D { n | data = Data.new Mobile.new doc.data }, toEngine Engine.stop )

        Undo ->
            ( D { doc | data = Data.undo doc.data }, Cmd.none )

        Redo ->
            ( D { doc | data = Data.redo doc.data }, Cmd.none )

        View l ->
            let
                mobileContent =
                    Content.M <| Data.current doc.data

                v =
                    cleanViewing l mobileContent
            in
            case getViewingHelper v mobileContent of
                Content.C c ->
                    ( D { doc | viewing = v, editor = C CEditor.init }, toEngine Engine.stop )

                Content.M m ->
                    ( D { doc | viewing = l, editor = M MEditor.init }, toEngine Engine.stop )

                _ ->
                    Debug.log "IMPOSSIBLE Cannot view Sound" ( D doc, Cmd.none )

        KeyPressed sh ->
            case ( sh, doc.editor ) of
                ( Nav, M _ ) ->
                    update (MobileMsg <| MEditor.ChangedMode MEditor.Nav) scale (D doc)

                ( Normal, M _ ) ->
                    update (MobileMsg <| MEditor.ChangedMode MEditor.Normal) scale (D doc)

                ( Tool i, M _ ) ->
                    case i of
                        1 ->
                            update (MobileMsg <| MEditor.ChangedTool <| MEditor.Play False) scale (D doc)

                        2 ->
                            update (MobileMsg <| MEditor.ChangedTool <| MEditor.Harmonize) scale (D doc)

                        3 ->
                            update (MobileMsg <| MEditor.ChangedTool <| MEditor.Edit) scale (D doc)

                        _ ->
                            ( D doc, Cmd.none )

                ( Play, M _ ) ->
                    update (MobileMsg <| MEditor.ToggleEngine) scale (D doc)

                ( Play, C _ ) ->
                    update (CollarMsg <| CEditor.ToggleEngine) scale (D doc)

                _ ->
                    ( D doc, Cmd.none )

        MobileMsg subMsg ->
            case ( doc.editor, getViewing (D doc) ) of
                ( M editor, Content.M mobile ) ->
                    let
                        res =
                            MEditor.update subMsg scale ( editor, mobile )

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
                            D { doc | data = data, editor = M res.model }
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
                                                    scale
                                                    newDoc
                                )
                        )
                    , Maybe.withDefault Cmd.none <| Maybe.map toEngine res.toEngine
                    )

                _ ->
                    Debug.log "IMPOSSIBLE MobileMsg while viewing no mobile" ( D doc, Cmd.none )

        CollarMsg subMsg ->
            case ( doc.editor, getViewing (D doc) ) of
                ( C editor, Content.C collar ) ->
                    let
                        ( newEditor, ( co, to ), engineCmd ) =
                            CEditor.update subMsg ( editor, collar )

                        newMobile =
                            updateCollar doc.viewing (always co) <| Data.current doc.data

                        data =
                            case to of
                                CEditor.Do ->
                                    Data.do newMobile doc.data

                                CEditor.Group ->
                                    Data.do newMobile doc.data

                                CEditor.NOOP ->
                                    doc.data
                    in
                    ( D { doc | data = data }
                    , case engineCmd of
                        Nothing ->
                            Cmd.none

                        Just cmd ->
                            toEngine cmd
                    )

                _ ->
                    Debug.log "IMPOSSIBLE CollarMsg while viewing no collar" ( D doc, Cmd.none )

        InteractEvent event ->
            update (MobileMsg <| MEditor.Interacted event) scale (D doc)


viewTop : Doc -> Element Msg
viewTop (D doc) =
    column [ width fill ]
        [ viewNav (D doc)
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
viewNav (D doc) =
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
viewBottom (D doc) =
    case doc.editor of
        M editor ->
            [ Element.map MobileMsg <| MEditor.viewExtraTools editor ]

        _ ->
            []


viewSide : Doc -> List (Element Msg)
viewSide (D doc) =
    case ( doc.editor, getViewing (D doc) ) of
        ( M editor, Content.M m ) ->
            List.map (Element.map MobileMsg) <| MEditor.viewDetails editor m

        _ ->
            []


viewContent (D doc) =
    case ( getViewing (D doc), doc.editor ) of
        ( Content.M m, M editor ) ->
            MEditor.viewContent ( editor, m )

        ( Content.C c, C editor ) ->
            always <| always <| CEditor.viewContent ( editor, c )

        _ ->
            always <| always <| [ Svg.text "Cannot edit Sound currently" ]


getViewing : Doc -> WContent
getViewing (D { viewing, data }) =
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



-- TODO Should clean View with each update of data, do it in update before case ?


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
