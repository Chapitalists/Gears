port module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Color
import Data.Common exposing (Identifier)
import Data.Pupil as Pupil
import Data.Wheel as Wheel exposing (Wheel)
import Editor.Interacting exposing (Interactable(..), Zone)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as D
import Json.Encode as E
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Random
import Simple.Animation as Animation exposing (Animation, Millis)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Sound exposing (Sound)
import SoundCard exposing (SoundCard)
import Task
import Time exposing (Posix, every)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types exposing (Length(..), Opacity(..))
import Url exposing (Url)
import Utils.Coll as Coll
import Utils.Interact as Interact exposing (Action(..), Event)
import Utils.Palette exposing (Palette(..), roundButton)
import Utils.PanSvg as PanSvg exposing (PanSvg)
import Utils.Panel as Panel exposing (Panel)
import Utils.Utils exposing (Size, WheelMod(..), defaultStyle, drawWheel, htmlId, toggleListElement, unmaybeMap)


port newSound : ( String, String ) -> Cmd msg


port soundOk : (D.Value -> msg) -> Sub msg


port testPlay : E.Value -> Cmd msg



-- TODO refactor existing Debug.log with "key" value
-- TODO check msg or Msg in types, if unused, maybe replace by x
-- TODO clean all module exposings decl
-- TODO is "No error handling in update, everything comes Checked before"
-- TODO    a good pattern ?
-- TODO change all debug and silent edge or fail (_/NOOP) to debug.log
-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = sub
        , view = view
        , onUrlRequest = always NOOP
        , onUrlChange = always NOOP
        }



-- MODEL


type alias Model =
    { screenSize : Size
    , workplane : PanSvg
    , sel : List Identifier

    --, doc : Doc.Model
    , soundCard : SoundCard
    , state : State
    , tools : Tools
    , interact : Interact.State Interactable Zone
    }


type alias Tools =
    { panels : List Panel
    , floating : List Vec2
    }



--type alias FloatingTool =
--    { pos : Vec2
--    , }


type State
    = Prologue AutoGear
    | Creating AutoGear Vec2 Float
    | Bubble Vec2 Float
    | Wheel Wheel


type alias AutoGear =
    { pos : Vec2
    , dur : Float
    , laps : Float
    }



--TODO deal rand prologue and Creating growth with onAnimationFrame
--TODO all times in millis !
--
--
--
--type alias Views =
--    { lib : P.ViewType
--    , menu : P.ViewType
--    , soundCard : P.ViewType
--    }
--type Panel
--    = Library
--    | Properties
--    | Tools
--    | Wave
--    | SoundCard
--    | Pack
--    | Menu
--initViews : Views
--initViews =
--    { lib = P.Full
--    , menu = P.Border
--    , soundCard = P.Shown
--    }


init : Size -> Url -> Nav.Key -> ( Model, Cmd Msg )
init screen url _ =
    let
        initDur =
            2000
    in
    ( { screenSize = screen
      , workplane =
            PanSvg.init workplaneId
                screen
                (vec2 0 0)
                initDur
      , sel = []

      --, doc = Doc.init <| Just url
      , soundCard = SoundCard.init
      , state = Prologue <| AutoGear (vec2 0 0) initDur (initDur * 2)
      , tools = Tools [] []
      , interact = Interact.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotScreenSize Size
      --| ViewLibChg FullPanel
      --| ViewMenuChg P.ViewType
      --| ViewSoundChg P.ViewType
    | WorkplaneMsg PanSvg.Msg
      --| DocMsg Doc.Msg
    | SoundCardMsg SoundCard.Msg
    | InteractMsg (Interact.Msg Interactable Zone)
    | RequestAutoGear
    | GotAutoGear AutoGear
    | UpdateCreating Float
    | OpenSound File
    | GotSoundURL String String
    | SoundLoaded (Result D.Error Sound)
    | NOOP
    | SKIP


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SKIP ->
            ( { model
                | state =
                    Wheel <|
                        makeWheel (vec2 0 0) 2000 <|
                            Sound.fakeSound 1000
              }
            , Cmd.none
            )

        GotScreenSize size ->
            ( { model
                | screenSize = size
                , workplane = PanSvg.update (PanSvg.NewSize size) model.workplane
              }
            , Cmd.none
            )

        --ViewMenuChg vt ->
        --    let
        --        views =
        --            model.views
        --    in
        --    ( { model | views = { views | menu = vt } }, Cmd.none )
        --
        --ViewSoundChg vt ->
        --    let
        --        views =
        --            model.views
        --    in
        --    ( { model | views = { views | soundCard = vt } }, Cmd.none )
        WorkplaneMsg subMsg ->
            ( { model | workplane = PanSvg.update subMsg model.workplane }
            , Cmd.none
            )

        --DocMsg subMsg ->
        --    let
        --        ( doc, cmd ) =
        --            Doc.update subMsg model.doc
        --    in
        --    ( { model | doc = doc }, Cmd.map DocMsg cmd )
        SoundCardMsg subMsg ->
            let
                ( sc, cmd ) =
                    SoundCard.update subMsg model.soundCard
            in
            ( { model | soundCard = sc }, Cmd.map SoundCardMsg cmd )

        InteractMsg subMsg ->
            let
                ( state, mayEvent ) =
                    Interact.update subMsg model.interact

                newModel =
                    { model | interact = state }
            in
            case mayEvent of
                Just e ->
                    manageInteractEvent newModel e

                Nothing ->
                    ( newModel, Cmd.none )

        RequestAutoGear ->
            let
                ratio =
                    toFloat model.screenSize.width / toFloat model.screenSize.height
            in
            ( model, Random.generate GotAutoGear <| randPrologue ratio )

        GotAutoGear g ->
            case model.state of
                Prologue _ ->
                    ( { model | state = Prologue g }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateCreating dd ->
            case model.state of
                Creating g p d ->
                    ( { model | state = Creating g p (d + dd) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OpenSound file ->
            ( model
            , Task.perform (GotSoundURL <| File.name file) <| File.toUrl file
            )

        GotSoundURL name url ->
            ( model
            , newSound ( name, url )
            )

        SoundLoaded result ->
            case result of
                Err e ->
                    let
                        _ =
                            Debug.log "Wrong sound format" e
                    in
                    ( model, Cmd.none )

                Ok sound ->
                    case model.state of
                        Bubble pos dur ->
                            ( { model | state = Wheel <| makeWheel pos dur sound }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        NOOP ->
            ( model, Cmd.none )



-- SUBS


sub : Model -> Sub Msg
sub { state, screenSize, interact } =
    ([ BE.onResize (\w h -> GotScreenSize { width = w, height = h })

     --, Sub.map DocMsg <| Doc.sub doc
     , Sub.map SoundCardMsg SoundCard.sub
     , Sub.map InteractMsg <| Interact.sub interact
     ]
        ++ (case state of
                Prologue g ->
                    [ every (g.dur + g.laps) <| always RequestAutoGear ]

                Creating _ _ _ ->
                    [ BE.onAnimationFrameDelta UpdateCreating ]

                _ ->
                    []
           )
    )
        |> Sub.batch



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Gears !"
    , body =
        --[ layout [ htmlId "svgResizeObserver" ] <|
        [ layout [ inFront <| viewTools model.tools ] <|
            Element.html <|
                S.svg
                    (List.map (Attr.map WorkplaneMsg)
                        (PanSvg.svgAttributes model.workplane)
                        ++ List.map (Attr.map InteractMsg)
                            (Interact.draggableEvents ISurface)
                    )
                <|
                    case model.state of
                        Prologue g ->
                            [ autoGear prologueAnimation g ]

                        Creating g p d ->
                            [ autoGear prologueAnimation g
                            , S.circle (gearAttrs p d) []
                            ]

                        Bubble p d ->
                            [ S.circle (gearAttrs p d) [] ]

                        Wheel w ->
                            let
                                mod =
                                    if List.isEmpty model.sel then
                                        None

                                    else
                                        Selected False

                                style =
                                    { defaultStyle | mod = mod }

                                fakeInteract =
                                    Just <| IWheel ( Coll.startId, [] )
                            in
                            [ Html.map InteractMsg <|
                                Wheel.view w style fakeInteract wheelId Nothing
                            ]
        ]
    }



{- }
                   el
                   (P.view
                           ( ViewMenuChg
                           , map DocMsg <| Doc.viewMenu model.doc
                           )
                           P.Top
                           model.views.menu
                           model.screenSize
                       :: P.view
                           ( ViewSoundChg
                           , column [] <|
                               (map SoundMsg <| SoundCard.view model.soundCard)
                                   :: (List.map (map DocMsg) <| Doc.viewPlay model.doc)
                           )
                           P.Left
                           model.views.soundCard
                           model.screenSize
                       :: [ height <| px model.screenSize.height
                          , width <| px model.screenSize.width
                          , htmlAttribute <| Attr.style "flex-direction" "row"
                          ]
                   )
                   (map DocMsg <|
                       Doc.view model.doc
                   )
       ]
   }
-}


viewTools : Tools -> Element Msg
viewTools { panels, floating } =
    el [ alignRight ] <|
        roundButton 30 True False Red <|
            Input.button
                [ centerX
                , centerY
                , Font.size 15
                ]
                { onPress = Just SKIP
                , label = text "SKIP"
                }


manageInteractEvent :
    Model
    -> Event Interactable Zone
    -> ( Model, Cmd Msg )
manageInteractEvent model event =
    let
        return =
            ( model, Cmd.none )
    in
    case model.state of
        Prologue g ->
            case ( event.item, event.action ) of
                ( ISurface, Start pos ) ->
                    let
                        p =
                            PanSvg.mapIn pos model.workplane
                    in
                    ( { model | state = Creating g p 0 }, Cmd.none )

                _ ->
                    return

        Creating g p t ->
            case ( event.item, event.action ) of
                ( ISurface, HoldEnded d ) ->
                    ( { model | state = Bubble p d }
                    , Select.file soundMimeTypes OpenSound
                    )

                ( ISurface, Clicked _ ) ->
                    ( { model | state = Prologue g }, Cmd.none )

                _ ->
                    return

        Wheel w ->
            case ( event.item, event.action ) of
                ( IWheel id, Clicked _ ) ->
                    ( { model | sel = toggleListElement id model.sel }
                    , Cmd.none
                    )

                _ ->
                    return

        Bubble _ _ ->
            return


makeWheel : Vec2 -> Float -> Sound -> Wheel
makeWheel pos dur sound =
    Wheel.fromSoundAndInterval sound
        dur
        { pos = pos
        , bgHue = 0
        , pupilHue = 0.8
        }


playWheel : Wheel -> E.Value
playWheel w =
    let
        wheel =
            Wheel.getEngined w

        mayPupil =
            Maybe.map Pupil.getEngined wheel.pupil
    in
    -- somewhat copied from Engine.encodeWheel / encodeGear
    E.object
        ([ ( "wheelId", E.string wheelId ) --TODO
         , ( "interval", E.float wheel.interval )
         , ( "mute", E.bool False ) --TODO
         , ( "volume", E.float 1 ) --TODO
         , ( "wheelStartPercent", E.float wheel.startPercent )
         , ( "view", E.bool True ) --TODO
         ]
            ++ unmaybeMap mayPupil
                []
                (\pupil ->
                    [ ( "pupilDuration", E.float pupil.duration )
                    , ( "pupilStartPercent", E.float pupil.startPercent )
                    ]
                        ++ (let
                                sound =
                                    Sound.getEngined pupil.sound
                            in
                            [ ( "soundPath", E.string sound.path )
                            , ( "soundPercents"
                              , E.list E.float
                                    [ sound.startPercent
                                    , sound.endPercent
                                    ]
                              )
                            ]
                           )
                )
        )


autoGear :
    (Float -> Animation)
    -> { a | pos : Vec2, dur : Float }
    -> Html msg
autoGear anim g =
    animatedSvg S.circle
        (anim g.dur)
        (gearAttrs g.pos 1)
        []


gearAttrs : Vec2 -> Float -> List (Svg.Attribute msg)
gearAttrs pos d =
    [ SA.cx <| Num <| getX pos
    , SA.cy <| Num <| getY pos
    , SA.r <| Num (d / 2)
    , SA.fillOpacity <| Opacity 0.5
    , SA.stroke Color.black
    , SA.strokeWidth <| Num (d / 30)
    ]


creatingAnimation : Float -> Animation
creatingAnimation dur =
    let
        lookahead =
            500
    in
    Animation.fromTo
        { duration = lookahead
        , options = [ Animation.linear ]
        }
        [ P.scale dur ]
        [ P.scale (dur + lookahead) ]


prologueAnimation : Float -> Animation
prologueAnimation dur =
    Animation.steps
        { startAt = [ P.scale 0, P.opacity 1 ]
        , options =
            [ Animation.count 1
            , Animation.linear
            ]
        }
        [ Animation.step (round dur) [ P.scale dur, P.opacity 1 ]
        , Animation.step 500 [ P.scale dur, P.opacity 0 ]
        ]


animatedSvg =
    Animated.svg
        { class = SA.class << List.singleton }


randPrologue : Float -> Random.Generator AutoGear
randPrologue ratio =
    let
        height =
            4000

        width =
            height * ratio

        y =
            Random.float -height height

        x =
            Random.float -width width

        dur =
            Random.int 1000 4000 |> Random.map toFloat

        laps =
            Random.int 1000 5000 |> Random.map toFloat
    in
    Random.map3 AutoGear (Random.map2 vec2 x y) dur laps


soundMimeTypes : List String
soundMimeTypes =
    [ "audio/x-wav", "audio/wav" ]


workplaneId : String
workplaneId =
    "mainWorkPlaneID"


wheelId : String
wheelId =
    "wheel-ID"


pupilId : String
pupilId =
    "pupil-ID"
