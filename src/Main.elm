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
import Http
import Interact
import Json.Decode as D
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Result exposing (Result)
import Set exposing (Set)
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SS
import TypedSvg.Types exposing (Length(..), Transform(..))
import Url exposing (Url)


port loadSound : String -> Cmd msg


port soundLoaded : (String -> msg) -> Sub msg


port newSVGSize : (D.Value -> msg) -> Sub msg



-- TODO refactor existing Debug.log with "key" value
-- TODO check bug visibility hidden not emitted on window change but on tab change
-- TODO check msg or Msg in types, if unused, maybe replace by x
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
    , details : Maybe (Id Gear)
    , interact : Interact.State String
    , debug : String -- TODO change all debug and silent edge or fail (_/NOOP) to debug.log
    }


type alias ViewPos =
    { c : Vec2, smallestSize : Float }


getScale : Model -> Float
getScale model =
    model.viewPos.smallestSize / min model.svgSize.height model.svgSize.width


type alias Size =
    { width : Float
    , height : Float
    }


sizeDecoder =
    D.decodeValue <| D.map2 Size (D.field "width" D.float) (D.field "height" D.float)


type alias ClickState =
    { target : Id Gear
    , drag : Bool
    , pos : Vec2
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    ( Model False url Set.empty [] Doc.new (ViewPos (vec2 0 0) 10) (Size 0 0) Nothing Interact.init ""
    , fetchSoundList url
    )



-- UPDATE


type Msg
    = GotSoundList (Result Http.Error String)
    | RequestSoundList
    | RequestSoundLoad String
    | SoundLoaded String
    | SoundClicked Sound
    | UpdateViewPos ViewPos
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
            let
                newList =
                    case String.split " " res of
                        name :: "ok" :: _ ->
                            Sound.fromPath name :: model.loadedSoundList

                        _ ->
                            model.loadedSoundList
            in
            ( { model | debug = res, loadedSoundList = newList }, Cmd.none )

        -- TODO strange interferences between Doc, Gear, ViewPos
        SoundClicked sound ->
            let
                ( newDoc, newGearPos ) =
                    Doc.addNewGear sound model.doc
            in
            ( { model | doc = newDoc, viewPos = { c = newGearPos, smallestSize = Sound.length sound * 2 * 4 } }, Cmd.none )

        {- }
           --TODO could be same message ? If no clickState, startClick, else, clickMove
           ClickMove pos ->
               case model.click of
                   Nothing ->
                       ( model, Cmd.none )

                   Just state ->
                       let
                           dPos =
                               Vec2.scale (getScale model) <| Vec2.sub pos state.pos
                       in
                       ( { model
                           | gears = Coll.update state.target (Gear.move dPos) model.gears
                           , click = Just { state | drag = True, pos = pos }
                         }
                       , Cmd.none
                       )

           EndClick ->
               case model.click of
                   Nothing ->
                       ( model, Debug.log "IMPOSSIBLE No click to end" Cmd.none )

                   Just { target, drag, pos } ->
                       let
                           newM =
                               -- TODO this modification should be in another module, or one or another, newM smells
                               { model | click = Nothing }
                       in
                       if drag then
                           ( newM, Cmd.none )

                       else
                           case model.tool of
                               Play ->
                                   case Coll.get target model.gears of
                                       Nothing ->
                                           ( newM, Debug.log ("IMPOSSIBLE No gear to play for id " ++ Gear.jsId target) Cmd.none )

                                       Just g ->
                                           ( { newM | gears = Coll.update target Gear.play model.gears }
                                           , toEngine <| engineEncoder { playable = SingleGear ( target, g ), action = PlayPause }
                                           )

                               Edit ->
                                   ( { newM | details = Just target }, Cmd.none )

                               Link ->
                                   ( newM, Cmd.none )

           AbortClick ->
               ( { model | click = Nothing }, Cmd.none )
        -}
        UpdateViewPos vp ->
            ( { model | viewPos = vp }, Cmd.none )

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

                ( doc, cmd ) =
                    Doc.update (Doc.InteractEvent event) model.doc
            in
            ( { model | interact = interact, doc = doc }, cmd )

        NOOP ->
            ( model, Cmd.none )

        Problem str ->
            ( { model | debug = str }, Cmd.none )



-- SUBS


subs { interact } =
    Sub.batch <|
        [ soundLoaded SoundLoaded
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
                ([ column [ height fill, Bg.color (rgb 0.5 0.5 0.5), Font.color (rgb 1 1 1), spacing 20, padding 10 ]
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
                 , column [ width fill, height fill ]
                    [ Element.map DocMsg <| Doc.viewTools model.doc
                    , el [ width fill, height fill ] <|
                        Element.html <|
                            S.svg
                                ([ Html.Attributes.id "svg"
                                 , SS.attribute "width" "100%"
                                 , SS.attribute "height" "100%"
                                 , SA.preserveAspectRatio TypedSvg.Types.AlignNone TypedSvg.Types.Meet
                                 , computeViewBox model
                                 ]
                                    ++ List.map (Html.Attributes.map InteractMsg)
                                        (Interact.dragSpaceEvents model.interact)
                                )
                            <|
                                List.map (SS.map forwardGearOutMsg) <|
                                    Doc.viewContent model.doc <|
                                        Interact.getInteract model.interact
                    ]
                 ]
                    ++ (case model.details of
                            Nothing ->
                                []

                            Just id ->
                                case Doc.getGear id model.doc of
                                    Nothing ->
                                        []

                                    Just g ->
                                        [ viewDetails id g ]
                       )
                )
        ]
    }


soundView : Sound -> Element Msg
soundView s =
    el
        [ onClick <| SoundClicked s ]
        (text (Sound.toString s))


viewDetails : Id Gear -> Gear -> Element Msg
viewDetails id g =
    column [] [ Input.button [] { onPress = Just <| DocMsg <| Doc.DeleteGear id, label = text "Supprimer" } ]


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
                Vec2.getX viewPos.c - w / 2

            y =
                Vec2.getY viewPos.c - h / 2
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
