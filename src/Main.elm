port module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Color
import Data.Common exposing (Identifier)
import Data.Pupil as Pupil
import Data.Wheel as Wheel exposing (IntervalOrPupil(..), Wheel)
import Editor.Interacting exposing (Interactable(..), Zone(..))
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input exposing (defaultThumb, labelHidden)
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
import TypedSvg.Types exposing (Length(..), Opacity(..), Transform(..))
import Url exposing (Url)
import Utils.Coll as Coll
import Utils.Gesture as Gesture exposing (Event(..), Gesture)
import Utils.Interact as Interact exposing (Action(..))
import Utils.Palette exposing (Palette(..), roundButton)
import Utils.PanSvg as PanSvg exposing (PanSvg)
import Utils.Panel as Panel exposing (Panel)
import Utils.Utils exposing (Size, defaultStyle, drawWheel, htmlId, toggleListElement, unmaybeMap)


port newSound : ( String, String ) -> Cmd msg


port soundOk : (D.Value -> msg) -> Sub msg


port testPlay : E.Value -> Cmd msg


port testStop : () -> Cmd msg



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
    , engine : Bool
    , gesture : Gesture
    }


type alias Tools =
    { panels : List Panel
    , floating : List Vec2
    , percent : Float
    }



--type alias FloatingTool =
--    { pos : Vec2
--    , }


type State
    = Prologue AutoGear
    | Creating AutoGear Vec2 Float
    | Bubble Vec2 Float
    | Wheel Wheel (Maybe ( Float, Wheel.IntervalOrPupil ))


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
      , tools = Tools [] [] 0
      , engine = False
      , gesture = Gesture.init
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
    | RequestAutoGear
    | GotAutoGear AutoGear
    | UpdateCreating Float
    | OpenSound File
    | GotSoundURL String String
    | SoundLoaded (Result D.Error Sound)
    | NOOP
    | SKIP
    | NewPercent Float
    | GestureMsg Gesture.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPercent p ->
            let
                tools =
                    model.tools
            in
            ( { model
                | state =
                    Wheel
                        (makeWheel (vec2 0 0) 2000 p <|
                            Sound.fakeSound 1000
                        )
                        Nothing
                , tools = { tools | percent = p }
              }
            , Cmd.none
            )

        SKIP ->
            ( { model
                | state =
                    Wheel
                        (makeWheel (vec2 0 0) 2000 0 <|
                            Sound.fakeSound 1000
                        )
                        Nothing
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
                            ( { model
                                | state =
                                    Wheel
                                        (makeWheel pos dur 0 sound)
                                        Nothing
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        NOOP ->
            ( model, Cmd.none )

        GestureMsg subMsg ->
            let
                { gesture, event, cmd, interactEvent } =
                    Gesture.update model.gesture subMsg

                newModel =
                    { model | gesture = gesture }

                interactResult =
                    Maybe.andThen
                        (manageInteractEvent newModel)
                        interactEvent

                addCmd =
                    Tuple.mapSecond
                        (\c ->
                            Cmd.batch
                                [ c, Cmd.map GestureMsg cmd ]
                        )
            in
            addCmd <|
                case interactResult of
                    Just res ->
                        res

                    Nothing ->
                        manageGestureEvent newModel event



-- SUBS


sub : Model -> Sub Msg
sub { state, screenSize, gesture } =
    ([ BE.onResize (\w h -> GotScreenSize { width = w, height = h })

     --, Sub.map DocMsg <| Doc.sub doc
     , Sub.map SoundCardMsg SoundCard.sub
     , Sub.map GestureMsg <| Gesture.sub gesture
     , soundOk (SoundLoaded << D.decodeValue Sound.decoder)
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
                        ++ List.map (Attr.map (GestureMsg << Gesture.Msg))
                            (Interact.draggableEvents Gesture.NoItem
                                ++ Interact.dragSpaceEvents Gesture.Workplane
                            )
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

                        Wheel w mayMod ->
                            viewWheelState model w mayMod
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
viewTools { percent } =
    el [ alignRight ] <|
        column []
            [ roundButton 30 True False Red <|
                Input.button
                    [ centerX
                    , centerY
                    , Font.size 15
                    ]
                    { onPress = Just SKIP
                    , label = text "SKIP"
                    }
            , Input.slider []
                { onChange = NewPercent
                , label = labelHidden "percent"
                , min = 0
                , max = 1
                , value = percent
                , thumb = defaultThumb
                , step = Nothing
                }
            ]


viewWheelState : Model -> Wheel -> Maybe ( Float, IntervalOrPupil ) -> List (Svg Msg)
viewWheelState model w mayMod =
    let
        style =
            { defaultStyle | selected = not <| List.isEmpty model.sel }

        gestDragAttrs =
            Gesture.attributes

        attrs =
            List.map (Attr.map GestureMsg) gestDragAttrs

        opacity =
            SA.opacity <| Opacity 0.2

        modView =
            case mayMod of
                Just ( scale, Interval ) ->
                    let
                        ( intervalX, intervalY ) =
                            Tuple.mapBoth ((*) scale) ((*) scale) <|
                                Wheel.getIntervalTranslate w
                    in
                    [ drawWheel
                        (Wheel.getInterval w * scale)
                        Nothing
                        { defaultStyle | thin = True }
                        "mod"
                        [ opacity
                        , SA.transform [ Translate intervalX intervalY ]
                        ]
                        []
                        []
                    ]

                Just ( scale, Pupil ) ->
                    unmaybeMap (Wheel.getPupil w) [] <|
                        \p ->
                            let
                                ( pupilX, pupilY ) =
                                    Tuple.mapBoth ((*) scale) ((*) scale) <|
                                        Wheel.getPupilTranslate w p
                            in
                            [ drawWheel
                                (Pupil.getDuration p * scale)
                                (Pupil.getColor p)
                                { defaultStyle | thin = True }
                                "mod"
                                [ opacity
                                , SA.transform [ Translate pupilX pupilY ]
                                ]
                                []
                                []
                            ]

                _ ->
                    []
    in
    Wheel.view w style attrs wheelId Nothing
        :: (List.map (Svg.map GestureMsg) <|
                Gesture.view
                    model.gesture
                    model.workplane
                    model.screenSize
                    w
           )
        ++ modView


manageGestureEvent : Model -> Maybe Event -> ( Model, Cmd Msg )
manageGestureEvent model event =
    case model.state of
        Wheel w mayMod ->
            let
                modScale =
                    (^) 2
                        << (\n -> n / 100)

                state =
                    case event of
                        Just (Up d) ->
                            Wheel w <|
                                Just ( modScale d, Interval )

                        Just (Down d) ->
                            Wheel w <|
                                Just ( modScale -d, Interval )

                        Just (Left d) ->
                            Wheel w <|
                                Just ( modScale -d, Pupil )

                        Just (Right d) ->
                            Wheel w <|
                                Just ( modScale d, Pupil )

                        Just (End validate) ->
                            case ( mayMod, validate ) of
                                ( Just ( scale, pupOrInt ), True ) ->
                                    case pupOrInt of
                                        Interval ->
                                            Wheel (Wheel.scaleInterval scale w)
                                                Nothing

                                        Pupil ->
                                            Wheel
                                                (Wheel.scalePupilDuration scale w)
                                                Nothing

                                _ ->
                                    Wheel w Nothing

                        _ ->
                            model.state
            in
            ( { model
                | state = state
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


manageInteractEvent :
    Model
    -> Interact.Event Gesture.Item Gesture.Zone
    -> Maybe ( Model, Cmd Msg )
manageInteractEvent model event =
    case model.state of
        Prologue g ->
            case ( event.item, event.action ) of
                ( Gesture.NoItem, Holded pos ) ->
                    let
                        p =
                            PanSvg.mapIn pos model.workplane
                    in
                    Just ( { model | state = Creating g p 0 }, Cmd.none )

                _ ->
                    Nothing

        Creating g p t ->
            case ( event.item, event.action ) of
                ( Gesture.NoItem, HoldEnded d ) ->
                    Just
                        ( { model | state = Bubble p d }
                        , Select.file soundMimeTypes OpenSound
                        )

                ( Gesture.NoItem, Clicked _ ) ->
                    Just ( { model | state = Prologue g }, Cmd.none )

                _ ->
                    Nothing

        Wheel w _ ->
            case ( event.item, event.action ) of
                ( Gesture.Item, Clicked _ ) ->
                    Just <| toEngine model model.engine w

                _ ->
                    Nothing

        Bubble _ _ ->
            Nothing


makeWheel : Vec2 -> Float -> Float -> Sound -> Wheel
makeWheel pos dur percent sound =
    Wheel.fromSoundAndInterval sound
        dur
        percent
        { pos = pos
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
        ([ ( "id", E.string wheelId ) --TODO
         , ( "interval", E.float <| wheel.interval / 1000 )
         , ( "mute", E.bool False ) --TODO
         , ( "volume", E.float 1 ) --TODO
         , ( "launchPercent", E.float wheel.launchPercent )
         , ( "view", E.bool True ) --TODO
         ]
            ++ unmaybeMap mayPupil
                []
                (\pupil ->
                    [ ( "pupil"
                      , E.object <|
                            [ ( "id", E.string (wheelId ++ Wheel.pupilID) )
                            , ( "length", E.float <| pupil.duration / 1000 )
                            , ( "volume", E.float 1 )
                            , ( "view", E.bool True )
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
                                    , ( "soundStartPercent"
                                      , E.float sound.loopPercent
                                      )
                                    ]
                                   )
                      )
                    ]
                )
        )


toEngine : Model -> Bool -> Wheel -> ( Model, Cmd msg )
toEngine model stop w =
    if stop then
        ( { model | engine = False }
        , testStop ()
        )

    else
        ( { model | engine = True }
        , testPlay <| playWheel w
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
