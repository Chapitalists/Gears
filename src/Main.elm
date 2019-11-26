port module Main exposing (..)

import Browser
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
    , svgSize : Size
    , interact : Interact.State Doc.Interactable
    }


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


type alias Size =
    { width : Float
    , height : Float
    }


sizeDecoder : D.Value -> Result D.Error Size
sizeDecoder =
    D.decodeValue <| D.map2 Size (D.field "width" D.float) (D.field "height" D.float)


type alias ClickState =
    { target : Id Gear
    , drag : Bool
    , pos : Vec2
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    ( Model False url Set.empty [] Set.empty (Doc.new <| Just url) (ViewPos (vec2 0 0) 10) (Size 0 0) Interact.init
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
    | GotLoadedFile (Result Http.Error Doc.Mobile)
    | SoundLoaded (Result D.Error Sound)
    | SoundClicked Sound
    | UpdateViewPos ViewPos
    | Zoom Float
    | GotSVGSize (Result D.Error Size)
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

        GotLoadedFile result ->
            case result of
                Ok m ->
                    ( { model
                        | connected = True
                      }
                    , Cmd.none
                    )

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

        UpdateViewPos vp ->
            ( { model | viewPos = vp }, Cmd.none )

        Zoom f ->
            let
                vp =
                    model.viewPos

                factor =
                    1 + f / 1000
            in
            ( { model | viewPos = { vp | smallestSize = model.viewPos.smallestSize * factor } }, Cmd.none )

        GotSVGSize res ->
            case res of
                Result.Err e ->
                    Debug.log (D.errorToString e) ( model, Cmd.none )

                Result.Ok s ->
                    ( { model | svgSize = s }, Cmd.none )

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
        [ soundLoaded (Sound.decoder >> SoundLoaded)
        , newSVGSize (sizeDecoder >> GotSVGSize)
        ]
            ++ List.map (Sub.map InteractMsg) (Interact.subs interact)



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
            row [ height fill, width fill ]
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
                     , Wheel.onWheel (Zoom << .deltaY)
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
        viewSoundLib model
            ++ viewSaveFiles model


viewSoundLib : Model -> List (Element Msg)
viewSoundLib model =
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
    , column [ spacing 5 ] <|
        text "Sons"
            :: (List.map (\s -> el [ onClick (RequestSoundLoad s) ] (text s)) <|
                    Set.toList model.soundList
               )
    , column [ spacing 10 ] <|
        text "Chargés"
            :: List.map soundView model.loadedSoundList
    ]


soundView : Sound -> Element Msg
soundView s =
    el
        [ onClick <| SoundClicked s ]
        (text (Sound.toString s))


viewSaveFiles : Model -> List (Element Msg)
viewSaveFiles model =
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
    , column [ spacing 5 ] <|
        text "Fichiers"
            :: (List.map (\s -> el [ onClick (RequestSaveLoad s) ] (text <| String.dropRight 6 s)) <|
                    Set.toList model.savesList
               )
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
        , expect = Http.expectJson GotLoadedFile <| D.succeed { gears = Coll.empty Gear.default, motor = Coll.startId }
        }
