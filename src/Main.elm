port module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Coll exposing (Coll, Id)
import Doc exposing (Doc, Interactable)
import Element exposing (..)
import Element.Background as Bg
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gear exposing (Gear)
import Html.Attributes
import Html.Events.Extra.Wheel as Wheel
import Http
import Interact
import Json.Decode as D
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Result exposing (Result)
import Set exposing (Set)
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SS
import TypedSvg.Types exposing (Length(..), Transform(..))
import Url exposing (Url)


port loadSound : String -> Cmd msg


port soundLoaded : (D.Value -> msg) -> Sub msg


port newSVGSize : (D.Value -> msg) -> Sub msg



-- TODO refactor existing Debug.log with "key" value
-- TODO check bug visibility hidden not emitted on window change but on tab change
-- TODO check msg or Msg in types, if unused, maybe replace by x
-- TODO clean all module exposings decl
-- TODO is "No error handling in update, everything comes Checked before" is a good pattern ?
-- TODO change all debug and silent edge or fail (_/NOOP) to debug.log
-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        , onUrlRequest = always NOOP
        , onUrlChange = always NOOP
        }



-- MODEL


type alias Model =
    { connected : Bool
    , currentUrl : Url.Url
    , soundList : Set String
    , loadedSoundList : List Sound
    , savesList : Set String
    , doc : Doc
    , viewPos : ViewPos
    , svgSize : Size Float
    , screenSize : Size Int
    , fileExplorerTab : ExTab
    , interact : Interact.State Doc.Interactable
    }


type ExTab
    = Sounds
    | Loaded
    | Saves


svgId : String
svgId =
    "svg"


type alias ViewPos =
    { c : Vec2, smallestSize : Float }


getScale : Model -> Float
getScale { viewPos, svgSize } =
    viewPos.smallestSize / min svgSize.height svgSize.width


posToSvg : Vec2 -> Model -> Vec2
posToSvg pos { viewPos, svgSize } =
    Vec.add
        viewPos.c
    <|
        Vec.scale
            (viewPos.smallestSize / min svgSize.height svgSize.width)
        <|
            Vec.sub
                pos
                (vec2 (svgSize.width / 2) (svgSize.height / 2))


type alias Size number =
    { width : number
    , height : number
    }


sizeDecoder : D.Value -> Result D.Error (Size Float)
sizeDecoder =
    D.decodeValue <| D.map2 Size (D.field "width" D.float) (D.field "height" D.float)


type alias ClickState =
    { target : Id Gear
    , drag : Bool
    , pos : Vec2
    }


init : Size Int -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init screen url _ =
    ( Model
        False
        url
        Set.empty
        []
        Set.empty
        (Doc.new <| Just url)
        (ViewPos (vec2 0 0) 10)
        (Size 0 0)
        screen
        Sounds
        Interact.init
    , fetchSoundList url
    )



-- UPDATE


type Msg
    = GotSoundList (Result Http.Error String)
    | RequestSoundList
    | RequestSoundLoad String
    | RequestSavesList
    | RequestSaveLoad String
    | GotSavesList (Result Http.Error String)
    | GotLoadedFile String (Result Http.Error Doc.Mobile)
    | SoundLoaded (Result D.Error Sound)
    | SoundClicked Sound
    | ChangedExplorerTab ExTab
    | UpdateViewPos ViewPos
    | Zoom Float ( Float, Float )
    | GotSVGSize (Result D.Error (Size Float))
    | GotScreenSize (Size Int)
    | DocMsg Doc.Msg
    | InteractMsg (Interact.Msg Doc.Interactable)
    | NOOP


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSoundList result ->
            case result of
                Ok stringList ->
                    ( { model
                        | soundList = Set.union model.soundList <| Set.fromList <| String.split "\\" stringList
                        , connected = True
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | connected = False }, Cmd.none )

        GotSavesList result ->
            case result of
                Ok stringList ->
                    ( { model
                        | savesList = Set.union model.savesList <| Set.fromList <| String.split "\\" stringList
                        , connected = True
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | connected = False, savesList = Set.empty }, Cmd.none )

        GotLoadedFile name result ->
            case result of
                Ok m ->
                    let
                        cmds =
                            Cmd.batch <|
                                List.map
                                    (\gear ->
                                        Tuple.second <|
                                            update
                                                (RequestSoundLoad <| Sound.toString <| gear.sound)
                                                model
                                    )
                                <|
                                    Coll.values m.gears
                    in
                    ( { model
                        | connected = True
                        , doc = Doc.changeMobile m name (Just model.currentUrl) model.doc
                        , viewPos =
                            { c = Gear.getPos <| Coll.get m.motor m.gears
                            , smallestSize = Gear.getLengthId m.motor m.gears * 2 * 4
                            }
                      }
                    , cmds
                    )

                Err (Http.BadBody err) ->
                    Debug.log err ( model, Cmd.none )

                Err _ ->
                    ( { model | connected = False }, Cmd.none )

        RequestSoundList ->
            ( model, fetchSoundList model.currentUrl )

        RequestSoundLoad n ->
            -- TODO handle no response
            ( model
            , if Set.member n model.soundList then
                loadSound n

              else
                Cmd.none
            )

        RequestSavesList ->
            ( model, fetchSavesList model.currentUrl )

        RequestSaveLoad n ->
            -- TODO handle no response
            ( model
            , if Set.member n model.savesList then
                fetchSaveFile model.currentUrl n

              else
                Cmd.none
            )

        SoundLoaded res ->
            case res of
                Result.Err e ->
                    Debug.log (D.errorToString e) ( model, Cmd.none )

                Result.Ok s ->
                    ( { model | loadedSoundList = s :: model.loadedSoundList }, Cmd.none )

        SoundClicked sound ->
            let
                ( newDoc, newGearPos ) =
                    Doc.soundClicked sound model.doc
            in
            ( { model
                | doc = newDoc
                , viewPos =
                    case newGearPos of
                        Just newPos ->
                            { c = newPos, smallestSize = Sound.length sound * 2 * 4 }

                        Nothing ->
                            model.viewPos
              }
            , Cmd.none
            )

        ChangedExplorerTab tab ->
            ( { model | fileExplorerTab = tab }, Cmd.none )

        UpdateViewPos vp ->
            ( { model | viewPos = vp }, Cmd.none )

        Zoom f ( x, y ) ->
            let
                vp =
                    model.viewPos

                factor =
                    1 + f / 1000

                p =
                    Vec.sub (posToSvg (vec2 x y) model) vp.c

                nS =
                    vp.smallestSize * factor

                scale =
                    nS / vp.smallestSize - 1

                nC =
                    Vec.sub vp.c <| Vec.scale scale p
            in
            ( { model | viewPos = { c = nC, smallestSize = nS } }, Cmd.none )

        GotSVGSize res ->
            case res of
                Result.Err e ->
                    Debug.log (D.errorToString e) ( model, Cmd.none )

                Result.Ok s ->
                    ( { model | svgSize = s }, Cmd.none )

        GotScreenSize size ->
            ( { model | screenSize = size }, Cmd.none )

        DocMsg subMsg ->
            let
                ( doc, cmd ) =
                    Doc.update subMsg (getScale model) model.doc
            in
            ( { model | doc = doc }, Cmd.map DocMsg cmd )

        InteractMsg subMsg ->
            let
                ( interact, event ) =
                    Interact.update subMsg model.interact
            in
            case event of
                Just e ->
                    let
                        svgEvent =
                            case e.action of
                                Interact.Dragged pos1 pos2 k ->
                                    { e | action = Interact.Dragged (posToSvg pos1 model) (posToSvg pos2 model) k }

                                _ ->
                                    e

                        ( doc, cmd ) =
                            Doc.update (Doc.InteractEvent svgEvent) (getScale model) model.doc
                    in
                    ( { model | interact = interact, doc = doc }, Cmd.map DocMsg cmd )

                Nothing ->
                    ( { model | interact = interact }, Cmd.none )

        NOOP ->
            ( model, Cmd.none )



-- SUBS


subs { interact } =
    Sub.batch <|
        [ soundLoaded (SoundLoaded << D.decodeValue Sound.decoder)
        , newSVGSize (sizeDecoder >> GotSVGSize)
        , BE.onKeyPress shortcutDecoder
        , BE.onResize (\w h -> GotScreenSize { width = w, height = h })
        ]
            ++ List.map (Sub.map InteractMsg) (Interact.subs interact)


shortcutDecoder : D.Decoder Msg
shortcutDecoder =
    D.map keyCodeToMsg <| D.field "code" D.string


keyCodeToMsg : String -> Msg
keyCodeToMsg str =
    case str of
        "KeyZ" ->
            DocMsg <| Doc.ChangedTool Doc.Play

        "KeyX" ->
            DocMsg <| Doc.ChangedTool Doc.Link

        "KeyC" ->
            DocMsg <| Doc.ChangedTool Doc.Edit

        "Space" ->
            DocMsg <| Doc.ToggleEngine

        _ ->
            NOOP



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title =
        "Gears !"
            ++ (if model.connected then
                    ""

                else
                    " - DISCONNECTED"
               )
    , body =
        [ layout [] <|
            row [ height <| px model.screenSize.height, width <| px model.screenSize.width ]
                ([ viewFileExplorer model
                 , viewDoc model
                 ]
                    ++ (Doc.viewDetails model.doc
                            |> List.map (Element.map DocMsg)
                       )
                )
        ]
    }


viewDoc : Model -> Element Msg
viewDoc model =
    column [ width fill, height fill ]
        [ Doc.viewTools model.doc
            |> Element.map DocMsg
        , el [ width fill, height fill, Element.htmlAttribute <| Html.Attributes.id "svgResizeObserver" ] <|
            Element.html <|
                S.svg
                    ([ Html.Attributes.id svgId
                     , SS.attribute "width" "100%"
                     , SS.attribute "height" "100%"
                     , SA.preserveAspectRatio TypedSvg.Types.AlignNone TypedSvg.Types.Meet
                     , computeViewBox model
                     , Wheel.onWheel (\e -> Zoom e.deltaY e.mouseEvent.offsetPos)
                     ]
                        ++ List.map (Html.Attributes.map InteractMsg)
                            (Interact.dragSpaceEvents model.interact)
                        ++ List.map (Html.Attributes.map InteractMsg)
                            (Interact.draggableEvents Doc.ISurface)
                    )
                <|
                    (Doc.viewContent model.doc (Interact.getInteract model.interact) (getScale model)
                        |> List.map (SS.map forwardGearOutMsg)
                    )
        , Doc.viewExtraTools model.doc
            |> Element.map DocMsg
        ]


viewFileExplorer : Model -> Element Msg
viewFileExplorer model =
    column [ height fill, Bg.color (rgb 0.5 0.5 0.5), Font.color (rgb 1 1 1), spacing 20, padding 10 ] <|
        ([ row [ Font.size 18, spacing 20 ]
            [ Input.button
                (if model.fileExplorerTab == Sounds then
                    [ padding 5, Bg.color (rgb 0.2 0.2 0.2) ]

                 else
                    [ padding 5 ]
                )
                { label = text "Sons"
                , onPress = Just <| ChangedExplorerTab Sounds
                }
            , Input.button
                (if model.fileExplorerTab == Loaded then
                    [ padding 5, Bg.color (rgb 0.1 0.1 0.1) ]

                 else
                    [ padding 5 ]
                )
                { label = text "Chargés"
                , onPress = Just <| ChangedExplorerTab Loaded
                }
            , Input.button
                (if model.fileExplorerTab == Saves then
                    [ padding 5, Bg.color (rgb 0.1 0.1 0.1) ]

                 else
                    [ padding 5 ]
                )
                { label = text "Saves"
                , onPress = Just <| ChangedExplorerTab Saves
                }
            ]
         ]
            ++ (case model.fileExplorerTab of
                    Sounds ->
                        viewSounds model

                    Loaded ->
                        viewLoaded model

                    Saves ->
                        viewSaveFiles model
               )
        )


viewSounds : Model -> List (Element Msg)
viewSounds model =
    [ column [ width fill, height <| fillPortion 2, spacing 20, scrollbarY ]
        [ Input.button
            [ Font.color <|
                if model.connected then
                    rgb 0 0 0

                else
                    rgb 1 0 0
            ]
            { onPress = Just RequestSoundList
            , label = text "Actualiser"
            }
        , column [ width fill, height <| fillPortion 1, spacing 5, padding 2, scrollbarY ] <|
            (List.map (\s -> el [ onClick (RequestSoundLoad s) ] (text s)) <|
                Set.toList model.soundList
            )
        ]
    ]


viewLoaded : Model -> List (Element Msg)
viewLoaded model =
    [ column [ width fill, height <| fillPortion 3, spacing 10, padding 2, scrollbarY ] <|
        List.map soundView model.loadedSoundList
    ]


soundView : Sound -> Element Msg
soundView s =
    el
        [ onClick <| SoundClicked s ]
        (text (Sound.toString s))


viewSaveFiles : Model -> List (Element Msg)
viewSaveFiles model =
    [ column [ height <| fillPortion 1, width fill, spacing 20 ]
        [ Input.button
            [ Font.color <|
                if model.connected then
                    rgb 0 0 0

                else
                    rgb 1 0 0
            ]
            { onPress = Just RequestSavesList
            , label = text "Actualiser"
            }
        , column [ width fill, spacing 5, padding 2, scrollbarY ] <|
            (List.map (\s -> el [ onClick (RequestSaveLoad s) ] (text <| cutGearsExtension s)) <|
                Set.toList model.savesList
            )
        ]
    ]


computeViewBox : Model -> SS.Attribute Msg
computeViewBox { viewPos, svgSize } =
    if svgSize.height == 0 || svgSize.width == 0 then
        SA.viewBox 0 0 100 100

    else
        let
            landscapeOrientation =
                svgSize.height < svgSize.width

            ratio =
                if landscapeOrientation then
                    svgSize.width / svgSize.height

                else
                    svgSize.height / svgSize.width

            h =
                viewPos.smallestSize

            w =
                h * ratio

            x =
                Vec.getX viewPos.c - w / 2

            y =
                Vec.getY viewPos.c - h / 2
        in
        if landscapeOrientation then
            SA.viewBox x y w h

        else
            SA.viewBox y x h w


forwardGearOutMsg : Interact.Msg Gear.Interactable -> Msg
forwardGearOutMsg msg =
    InteractMsg <| Interact.map Doc.fromGearInteractable msg



-- HTTP


fetchSoundList : Url.Url -> Cmd Msg
fetchSoundList url =
    Http.get
        { url = Url.toString { url | path = "/soundList" }
        , expect = Http.expectString GotSoundList
        }


fetchSavesList : Url.Url -> Cmd Msg
fetchSavesList url =
    Http.get
        { url = Url.toString { url | path = "/savesList" }
        , expect = Http.expectString GotSavesList
        }


fetchSaveFile : Url.Url -> String -> Cmd Msg
fetchSaveFile url name =
    Http.get
        { url = Url.toString { url | path = "/saves/" ++ name }
        , expect = Http.expectJson (GotLoadedFile <| cutGearsExtension name) Doc.mobileDecoder
        }


cutGearsExtension : String -> String
cutGearsExtension =
    String.dropRight 6
