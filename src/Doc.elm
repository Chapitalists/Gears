port module Doc exposing (..)

import Data exposing (Data)
import Data.Common as Common exposing (Identifier)
import Data.Content as Content exposing (Content)
import Data.Mobile as Mobile exposing (Geer, Mobeel)
import Data.Wheel as Wheel exposing (Conteet, Wheel)
import Dict exposing (Dict)
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
    , channels : Int
    }


getChannels :
    Doc
    -> Maybe Int
    -> ( Int, Bool ) -- True if real, False if Virtual
getChannels { channels } maxChan =
    case maxChan of
        Nothing ->
            ( channels, False )

        Just chan ->
            ( chan, True )


init : Maybe Url -> Model
init url =
    { data = Data.init (Doc Mobile.new "" 0) url
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
    | Pack
    | Editor Editor.Msg


type Msg
    = EnteredFileName String
    | EnteredChannels String
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
    | MobileMsg Editor.Msg -- FIXME Same as Mobile.InteractMsg ? Or What For ?
    | InteractMsg (Interact.Msg Interactable Zone)


update : Msg -> ( Model, Maybe Int ) -> ( Model, Cmd Msg )
update msg ( doc, maxChan ) =
    -- TODO Maybe clean viewing right here
    case msg of
        EnteredFileName name ->
            if String.all (\c -> Char.isAlphaNum c || c == '-') name then
                ( { doc | data = Data.setName name doc.data }, Cmd.none )

            else
                ( doc, Cmd.none )

        EnteredChannels str ->
            case maxChan of
                Just _ ->
                    let
                        _ =
                            Debug.log "Should’nt be able to enter Channels" str
                    in
                    ( doc, Cmd.none )

                Nothing ->
                    let
                        oldData =
                            Data.current doc.data
                    in
                    ( { doc
                        | data =
                            Data.do
                                { oldData
                                    | channels =
                                        Maybe.withDefault 0 <| String.toInt str
                                }
                                doc.data
                      }
                    , Cmd.none
                    )

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
                | data = Data.new (Doc Mobile.new "" 0) doc.data
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
            update
                (MobileMsg <| Editor.NewGear Editor.defaultAddPos content)
                ( doc, maxChan )

        KeyPressed sh ->
            let
                reUp mess =
                    update mess ( doc, maxChan )
            in
            case sh of
                Pack ->
                    reUp <|
                        MobileMsg <|
                            Editor.PackMsg <|
                                Pack.TogglePack

                Tool i ->
                    case i of
                        1 ->
                            reUp <|
                                MobileMsg <|
                                    Editor.ChangedTool <|
                                        Editor.Play False False

                        2 ->
                            reUp <|
                                MobileMsg <|
                                    Editor.ChangedTool <|
                                        Editor.Harmonize

                        3 ->
                            reUp <|
                                MobileMsg <|
                                    Editor.ChangedTool <|
                                        Editor.Edit False

                        _ ->
                            ( doc, Cmd.none )

                Play ->
                    case doc.editor.tool of
                        Editor.Play _ _ ->
                            reUp <| MobileMsg <| Editor.ToggleEngine

                        Editor.Edit True ->
                            reUp <| MobileMsg <| Editor.StopGear

                        Editor.Edit False ->
                            reUp <| MobileMsg <| Editor.PlayGear

                        _ ->
                            ( doc, Cmd.none )

                Left ->
                    reUp <| MobileMsg <| Editor.CursorLeft

                Right ->
                    reUp <| MobileMsg <| Editor.CursorRight

                Editor subMsg ->
                    reUp <| MobileMsg <| subMsg

        DirectionRepeat dir ->
            update (MobileMsg <| Editor.SvgMsg <| PanSvg.Pan dir) ( doc, maxChan )

        MobileMsg subMsg ->
            let
                mobile =
                    getViewing doc

                res =
                    Editor.update
                        ( getChannels (Data.current doc.data) maxChan
                        , Data.getName doc.data
                        )
                        subMsg
                        ( doc.editor, mobile )

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
                                                ( newDoc, maxChan )

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
            update (MobileMsg <| Editor.InteractMsg subMsg) ( doc, maxChan )


subs : Model -> List (Sub Msg)
subs doc =
    List.map (Sub.map MobileMsg) <| Editor.subs doc.editor


keyCodeToMode : List ( String, Editor.Mode )
keyCodeToMode =
    Editor.keyCodeToMode


keyCodeToShortcut : Model -> Dict String Shortcut
keyCodeToShortcut model =
    Dict.map (always Editor) <| Editor.keyCodeToShortcut model.editor <| getViewing model


keyCodeToDirection : Dict String Msg
keyCodeToDirection =
    Dict.map (always MobileMsg) Editor.keyCodeToDirection


view : ( Model, Maybe Int ) -> Element Msg
view ( doc, maxChan ) =
    row [ height fill, width fill ] <|
        (column [ width fill, height fill ]
            ([ viewTop doc
             , el
                [ width fill
                , height fill
                , Element.htmlAttribute <| Html.Attributes.id "svgResizeObserver"

                -- THX to https://discourse.elm-lang.org/t/elm-ui-parent-element-grows-to-encompass-children-instead-of-scrolling/5032
                , clip
                , htmlAttribute <| Html.Attributes.style "flex-shrink" "1"
                ]
               <|
                viewContent doc
             ]
                ++ viewBottom ( doc, maxChan )
            )
            :: viewSide ( doc, maxChan )
        )


viewTop : Model -> Element Msg
viewTop doc =
    let
        fontSize =
            14

        buttonSize =
            30

        inputSize =
            500
    in
    row [ Font.size fontSize ]
        [ roundButton buttonSize (Data.canUndo doc.data) False Yellow <|
            Input.button
                [ Font.size buttonSize
                , moveUp 2
                , moveLeft 4
                ]
                { label = text "⟲"

                --⎌"
                , onPress =
                    if Data.canUndo doc.data then
                        Just Undo

                    else
                        Nothing
                }
        , roundButton buttonSize (Data.canRedo doc.data) False Orange <|
            Input.button
                [ Font.size buttonSize
                , moveUp 2
                ]
                { label = text "⟳"
                , onPress =
                    if Data.canRedo doc.data then
                        Just Redo

                    else
                        Nothing
                }
        , roundButton buttonSize True False Green <|
            Input.button []
                { label =
                    el
                        [ Font.size buttonSize
                        , moveUp 4
                        , moveLeft 2
                        ]
                    <|
                        text "⍟"

                --"./icons/noun-wipe-2032492.svg"
                , onPress = Just New
                }
        , roundButton buttonSize (not <| Data.isSaved doc.data) False Red <|
            Input.button
                [ Font.size buttonSize
                , Font.semiBold
                , moveUp 1
                , moveLeft 1
                ]
                { label = text "🖫"
                , onPress = Just Save
                }
        , Input.text
            [ width (fill |> maximum inputSize)
            , padding 2
            ]
            { label = Input.labelHidden "Nom du fichier"
            , text = Data.getName doc.data
            , placeholder = Just <| Input.placeholder [] <| text "nom-a-sauvegarder"
            , onChange = EnteredFileName
            }
        ]


viewNav : Model -> Element Msg
viewNav doc =
    row
        [ width fill, padding 10, spacing 5 ]
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


viewBottom : ( Model, Maybe Int ) -> List (Element Msg)
viewBottom ( doc, maxChan ) =
    [ Editor.viewExtraTools MobileMsg doc.editor <| viewChannels ( doc, maxChan ) ]


viewSide : ( Model, Maybe Int ) -> List (Element Msg)
viewSide ( doc, maxChan ) =
    let
        channels =
            Tuple.first <| getChannels (Data.current doc.data) maxChan
    in
    List.map (Element.map MobileMsg) <|
        Editor.viewDetails channels doc.editor <|
            getViewing doc


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


viewChannels : ( Model, Maybe Int ) -> Element Msg
viewChannels ( { data }, maxChan ) =
    let
        doc =
            Data.current data

        ( channels, real ) =
            getChannels doc maxChan

        fontColor =
            if real then
                rgb 0.8 0.5 0.2

            else
                rgb 0 0 0
    in
    Input.text
        [ centerX
        , paddingXY 4 0
        , width <| minimum 60 <| maximum 100 <| fill
        , Font.color fontColor
        , htmlAttribute <| Html.Attributes.type_ "number"
        , htmlAttribute <| Html.Attributes.min "0"
        , htmlAttribute <| Html.Attributes.disabled real
        ]
        { onChange = EnteredChannels
        , text = String.fromInt channels
        , placeholder = Nothing
        , label = Input.labelLeft [] <| text "Canaux :"
        }


encoder : Doc -> E.Value
encoder d =
    E.object
        [ ( "mobile", Mobile.encoder d.mobile )
        , ( "comment", E.string d.comment )
        , ( "channels", E.int d.channels )
        ]


decoder : D.Decoder Doc
decoder =
    Field.attempt "mobile" Mobile.decoder <|
        \mayMobile ->
            case mayMobile of
                Just mobile ->
                    Field.attempt "channels" D.int <|
                        \mayChannels ->
                            Field.attempt "comment" D.string <|
                                \mayComment ->
                                    D.succeed <|
                                        Doc mobile
                                            (Maybe.withDefault "" mayComment)
                                            (Maybe.withDefault 0 mayChannels)

                Nothing ->
                    Mobile.decoder |> D.andThen (\m -> D.succeed <| Doc m "" 0)


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
                    let
                        _ =
                            Debug.log ("No mobile to view in " ++ str) mob
                    in
                    ViewRes mob "" []

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
                    let
                        _ =
                            Debug.log "IMPOSSIBLE View isn’t correct, should’ve been cleaned" ( l, mobile )
                    in
                    mobile

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
