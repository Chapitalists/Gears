port module Doc exposing (..)

import Coll exposing (Coll, Id)
import Content
import Data exposing (Data)
import Editor.Mobile as MEditor
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gear exposing (Gear)
import Harmony as Harmo
import Interact
import Json.Encode as E
import Math.Vector2 exposing (Vec2, vec2)
import Mobile exposing (Geer, Mobeel)
import Sound exposing (Sound)
import Url exposing (Url)
import Wheel exposing (Wheel)


port toEngine : E.Value -> Cmd msg


type Doc
    = D
        { data : Data Mobeel
        , viewing : List ( String, Identifier )
        , editor : MEditor.Model
        }


type Identifier
    = G (Id Geer)


changeMobile : Mobeel -> String -> Maybe Url -> Doc -> Doc
changeMobile m name url (D d) =
    D { d | data = Data.load m name url }


new : Maybe Url -> Doc
new url =
    D
        { data = Data.init { motor = Coll.startId, gears = Coll.empty Gear.typeString Mobile.defaultGear } url
        , viewing = []
        , editor = MEditor.init
        }



-- TODO Should be in Mobile.update, but shouldn’t return pos for that
-- Possible when viewPos moves to Mobile


soundClicked : Sound -> Doc -> ( Doc, Maybe Vec2 )
soundClicked sound (D doc) =
    case doc.editor.edit of
        MEditor.ChangeSound id ->
            let
                editor =
                    doc.editor

                mobile =
                    getViewing doc.viewing <| Data.current doc.data

                group =
                    Harmo.getHarmonicGroup (Coll.idMap id) mobile.gears

                chSound =
                    Wheel.update <| Wheel.ChangeContent <| Content.S sound
            in
            ( D
                { doc
                    | data =
                        Data.do
                            (updateViewing doc.viewing
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
                    , editor = { editor | edit = MEditor.Gear id }
                }
            , Nothing
            )

        _ ->
            let
                pos =
                    vec2 50 50
            in
            ( D
                { doc
                    | data =
                        Data.do
                            (updateViewing doc.viewing
                                (\m ->
                                    { m | gears = Coll.insert (Mobile.fromSound sound pos) m.gears }
                                )
                             <|
                                Data.current doc.data
                            )
                            doc.data
                }
            , Just pos
            )


type Msg
    = EnteredFileName String
    | Save
    | Saved
    | Undo
    | Redo
    | View (List ( String, Identifier ))
    | MobileMsg MEditor.Msg
    | InteractEvent (Interact.Event MEditor.Interactable)


update : Msg -> Float -> Doc -> ( Doc, Cmd Msg )
update msg scale (D doc) =
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

        Undo ->
            ( D { doc | data = Data.undo doc.data }, Cmd.none )

        Redo ->
            ( D { doc | data = Data.redo doc.data }, Cmd.none )

        View l ->
            ( D { doc | viewing = l }, Cmd.none )

        MobileMsg subMsg ->
            let
                ( editor, ( mo, to ), engineCmd ) =
                    MEditor.update subMsg scale ( doc.editor, getViewing doc.viewing <| Data.current doc.data )

                mobile =
                    updateViewing doc.viewing (always mo) <| Data.current doc.data

                data =
                    case to of
                        MEditor.Do ->
                            Data.do mobile doc.data

                        MEditor.Group ->
                            Data.group mobile doc.data

                        MEditor.NOOP ->
                            doc.data
            in
            ( D { doc | data = data, editor = editor }
            , case engineCmd of
                Nothing ->
                    Cmd.none

                Just v ->
                    toEngine v
            )

        InteractEvent event ->
            update (MobileMsg <| MEditor.Interacted event) scale (D doc)


viewTop : Doc -> Element Msg
viewTop (D doc) =
    column [ width fill ]
        [ viewNav (D doc)
        , row [ width fill, padding 10, spacing 20, Font.size 14 ]
            (Element.map MobileMsg (MEditor.viewTools doc.editor)
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
                            , onPress = Just <| View <| List.take i doc.viewing
                            }
                    )
                    doc.viewing


viewBottom : Doc -> Element Msg
viewBottom (D doc) =
    Element.map MobileMsg <| MEditor.viewExtraTools doc.editor


viewSide : Doc -> List (Element Msg)
viewSide (D doc) =
    List.map (Element.map MobileMsg) <| MEditor.viewDetails doc.editor <| Data.current doc.data


viewContent (D doc) =
    MEditor.viewContent ( doc.editor, Data.current doc.data )


getViewing : List ( String, Identifier ) -> Mobeel -> Mobeel
getViewing l m =
    case l of
        [] ->
            m

        ( _, G next ) :: rest ->
            case Wheel.getContent <| Coll.get next m.gears of
                Content.M mo ->
                    getViewing rest mo

                _ ->
                    Debug.log "IMPOSSIBLE Content not viewable" m


updateViewing : List ( String, Identifier ) -> (Mobeel -> Mobeel) -> Mobeel -> Mobeel
updateViewing l f m =
    case l of
        [] ->
            f m

        ( _, G next ) :: rest ->
            case Wheel.getContent <| Coll.get next m.gears of
                Content.M mo ->
                    { m
                        | gears =
                            Coll.update next
                                (Wheel.setContent <| Content.M <| updateViewing rest f mo)
                                m.gears
                    }

                _ ->
                    Debug.log "IMPOSSIBLE Content not Mobile" m
