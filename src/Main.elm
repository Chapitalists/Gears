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
    , doc : Doc
    , viewPos : ViewPos
    , svgSize : Size
    , interact : Interact.State String
    , debug : String -- TODO change all debug and silent edge or fail (_/NOOP) to debug.log
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
    ( Model False url Set.empty [] Doc.new (ViewPos (vec2 0 0) 10) (Size 0 0) Interact.init ""
    , fetchSoundList url
    )



-- UPDATE


type Msg
    = GotSoundList (Result Http.Error String)
    | RequestSoundList
    | RequestSoundLoad String
    | SoundLoaded (Result D.Error Sound)
    | SoundClicked Sound
    | UpdateViewPos ViewPos
    | Zoom Float
    | GotSVGSize (Result D.Error Size)
    | DocMsg Doc.Msg
    | InteractMsg (Interact.Msg String)
    | NOOP
    | Problem String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSoundList result ->
            case result of
                Ok stringList ->
                    ( { model
                        | soundList = Set.union model.soundList <| Set.fromList <| String.split " " stringList
                        , connected = True
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

        SoundLoaded res ->
            case res of
                Result.Err e ->
                    ( { model | debug = D.errorToString e }, Cmd.none )

                Result.Ok s ->
                    ( { model | loadedSoundList = s :: model.loadedSoundList }, Cmd.none )

        SoundClicked sound ->
            let
                ( newDoc, newGearPos ) =
                    Doc.addNewGear sound model.doc
            in
            ( { model | doc = newDoc, viewPos = { c = newGearPos, smallestSize = Sound.length sound * 2 * 4 } }, Cmd.none )

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
                    ( { model | debug = D.errorToString e }, Cmd.none )

                Result.Ok s ->
                    ( { model | svgSize = s }, Cmd.none )

        DocMsg subMsg ->
            let
                ( doc, cmd ) =
                    Doc.update subMsg model.doc
            in
            ( { model | doc = doc }, cmd )

        InteractMsg subMsg ->
            let
                ( interact, event ) =
                    Interact.update subMsg model.interact

                svgEvent =
                    case event of
                        Interact.Dragged item pos1 pos2 ->
                            Interact.Dragged item (posToSvg pos1 model) (posToSvg pos2 model)

                        _ ->
                            event

                ( doc, cmd ) =
                    Doc.update (Doc.InteractEvent svgEvent) model.doc
            in
            ( { model | interact = interact, doc = doc }, cmd )

        NOOP ->
            ( model, Cmd.none )

        Problem str ->
            ( { model | debug = str }, Cmd.none )



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
                ([ viewSoundLib model
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
                    ([ Html.Attributes.id "svg"
                     , SS.attribute "width" "100%"
                     , SS.attribute "height" "100%"
                     , SA.preserveAspectRatio TypedSvg.Types.AlignNone TypedSvg.Types.Meet
                     , computeViewBox model
                     , Wheel.onWheel (Zoom << .deltaY)
                     ]
                        ++ List.map (Html.Attributes.map InteractMsg)
                            (Interact.dragSpaceEvents model.interact)
                        ++ List.map (Html.Attributes.map InteractMsg)
                            (Interact.draggableEvents svgId)
                    )
                <|
                    (Doc.viewContent model.doc (Interact.getInteract model.interact)
                        |> List.map (SS.map forwardGearOutMsg)
                    )
        , Doc.viewExtraTools model.doc
            |> Element.map DocMsg
        ]


viewSoundLib : Model -> Element Msg
viewSoundLib model =
    column [ height fill, Bg.color (rgb 0.5 0.5 0.5), Font.color (rgb 1 1 1), spacing 20, padding 10 ]
        [ text <|
            if String.isEmpty model.debug then
                "Fine"

            else
                model.debug
        , Input.button
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


forwardGearOutMsg : Gear.OutMsg -> Msg
forwardGearOutMsg msg =
    case msg of
        Gear.InteractMsg m ->
            InteractMsg m

        Gear.GearMsg m ->
            DocMsg <| Doc.GearMsg m



-- HTTP


fetchSoundList : Url.Url -> Cmd Msg
fetchSoundList url =
    Http.get
        { url = Url.toString { url | path = "/soundList" }
        , expect = Http.expectString GotSoundList
        }
