module Main exposing (..)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
import Color
import Doc exposing (Doc)
import Editor.Interacting exposing (Interactable(..), Zone)
import Element exposing (..)
import Html exposing (Html)
import Html.Attributes as Attr
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Random
import Simple.Animation as Animation exposing (Animation, Millis)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import SoundCard exposing (SoundCard)
import Time exposing (Posix, every)
import Tools.Interact as Interact exposing (Action(..), Event)
import Tools.PanSvg as PanSvg exposing (PanSvg)
import Tools.Utils exposing (Size)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types exposing (Length(..), Opacity(..))
import Url exposing (Url)



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

    --, doc : Doc.Model
    , soundCard : SoundCard
    , state : State
    , interact : Interact.State Interactable Zone
    }


type State
    = Prologue AutoGear
    | Creating AutoGear Vec2 Float
    | Bubble Vec2 Float


type alias AutoGear =
    { pos : Vec2
    , dur : Float
    , laps : Float
    }



--TODO deal rand prologue and Creating growth with onAnimationFrame
--TODO all times in millis !
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

      --, doc = Doc.init <| Just url
      , soundCard = SoundCard.init
      , state = Prologue <| AutoGear (vec2 0 0) initDur (initDur * 2)
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
    | SoundMsg SoundCard.Msg
    | InteractMsg (Interact.Msg Interactable Zone)
    | RequestAutoGear
    | GotAutoGear AutoGear
    | UpdateCreating Float
    | NOOP


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        SoundMsg subMsg ->
            let
                ( sc, cmd ) =
                    SoundCard.update subMsg model.soundCard
            in
            ( { model | soundCard = sc }, Cmd.map SoundMsg cmd )

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

        NOOP ->
            ( model, Cmd.none )



-- SUBS


sub : Model -> Sub Msg
sub { state, screenSize, interact } =
    ([ BE.onResize (\w h -> GotScreenSize { width = w, height = h })

     --, Sub.map DocMsg <| Doc.sub doc
     , Sub.map SoundMsg SoundCard.sub
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
        [ layout [] <|
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
                    let
                        _ =
                            Debug.log "d t" ( d, t )
                    in
                    ( { model | state = Bubble p t }, Cmd.none )

                ( ISurface, Clicked _ ) ->
                    ( { model | state = Prologue g }, Cmd.none )

                _ ->
                    return

        _ ->
            return


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


workplaneId : String
workplaneId =
    "mainWorkPlaneID"
