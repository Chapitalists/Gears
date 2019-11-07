port module Main exposing (..)

import Browser
import Browser.Dom as DOM
import Browser.Events as BE
import Browser.Navigation as Nav
import Color
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Bg
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as D
import Json.Encode as E
import Result exposing (Result)
import Set exposing (Set)
import Task
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SS
import TypedSvg.Types exposing (Length(..), Transform(..))
import Url exposing (Url)


port loadSound : String -> Cmd msg


port toEngine : E.Value -> Cmd msg


type EngineAction
    = PlayPause
    | StopReset


actionToString a =
    case a of
        PlayPause ->
            "playPause"

        StopReset ->
            "stopReset"


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
    , loadedSoundList : List SoundFile
    , tool : Tool
    , gears : Dict Id Gear
    , viewPos : ViewPos
    , svgSize : Size
    , nextId : Int
    , details : Maybe Id
    , hover : Maybe Id
    , click : Maybe ClickState
    , debug : String -- TODO change all debug and silent edge or fail (_/NOOP) to debug.log
    }


type alias ViewPos =
    { cx : Float, cy : Float, smallestSize : Float }


getScale : Model -> Float
getScale model =
    model.viewPos.smallestSize / min model.svgSize.height model.svgSize.width


type SoundFile
    = Path String


sFtoString : SoundFile -> String
sFtoString (Path name) =
    name


sFtoTime : SoundFile -> Float
sFtoTime _ =
    1


type Tool
    = Edit
    | Play
    | Link


type Playable
    = SingleGear Gear


type alias Gear =
    { length : Float
    , x : Float
    , y : Float
    , startPercent : Float
    , stopped : Bool
    , sound : SoundFile
    }


moveGear : Float -> ( Float, Float ) -> Gear -> Gear
moveGear scale ( dx, dy ) g =
    { g | x = g.x + dx * scale, y = g.y + dy * scale }


type alias Id =
    Int


idToString id =
    "gear-" ++ String.fromInt id


engineEncoder : { action : EngineAction, id : Id, gear : Maybe Gear } -> E.Value
engineEncoder { action, id, gear } =
    E.object
        ([ ( "type", E.string "gear" )
         , ( "id", E.string <| idToString id )
         , ( "action", E.string <| actionToString action )
         ]
            ++ (case gear of
                    Nothing ->
                        []

                    Just g ->
                        [ ( "soundName", E.string <| sFtoString g.sound ) ]
               )
        )


type alias Size =
    { width : Float
    , height : Float
    }


sizeDecoder =
    D.decodeValue <| D.map2 Size (D.field "width" D.float) (D.field "height" D.float)


type alias ClickState =
    { target : Id
    , drag : Bool
    , pos : Coords
    }


howInteract id { hover, click } =
    case ( hover, click ) of
        ( Just target, Nothing ) ->
            if target == id then
                Hover

            else
                None

        ( _, Just { target, drag } ) ->
            if target == id then
                if drag then
                    Drag

                else
                    Click

            else
                None

        _ ->
            None


type Interact
    = None
    | Hover
    | Click
    | Drag


type alias Coords =
    ( Float, Float )


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    ( Model False url Set.empty [] Play Dict.empty (ViewPos 0 0 10) (Size 0 0) 1 Nothing Nothing Nothing ""
    , fetchSoundList url
    )



-- UPDATE


type Msg
    = NewSoundList (Result Http.Error String)
    | UpdateSoundList
    | ChangeTool Tool
    | RequestSoundLoad String
    | SoundLoaded String
    | CreateGear SoundFile
    | UpdateViewPos ViewPos
    | SVGSize (Result D.Error Size)
    | HoverIn Id
    | HoverOut
    | StartClick Id Mouse.Event
    | ClickMove Mouse.Event
    | EndClick
    | AbortClick
    | StopGear Id
    | DeleteGear Id
    | NOOP
    | Problem String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSoundList result ->
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

        UpdateSoundList ->
            ( model, fetchSoundList model.currentUrl )

        ChangeTool t ->
            ( { model | tool = t }, Cmd.none )

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
                        name :: "ok" :: rest ->
                            Path name :: model.loadedSoundList

                        _ ->
                            model.loadedSoundList
            in
            ( { model | debug = res, loadedSoundList = newList }, Cmd.none )

        CreateGear sf ->
            let
                x =
                    50

                y =
                    50

                length =
                    sFtoTime sf
            in
            ( { model
                | gears = Dict.insert model.nextId (Gear length x y 0.2 True sf) model.gears
                , nextId = model.nextId + 1
                , viewPos = { cx = x, cy = y, smallestSize = length * 2 * 4 }
              }
            , Cmd.none
            )

        HoverIn id ->
            ( { model | hover = Just id }, Cmd.none )

        HoverOut ->
            ( { model | hover = Nothing }, Cmd.none )

        StartClick id e ->
            ( { model | click = Just (ClickState id False e.clientPos) }, Cmd.none )

        ClickMove e ->
            case model.click of
                Nothing ->
                    ( model, Cmd.none )

                Just state ->
                    let
                        scale =
                            getScale model

                        move =
                            moveGear scale <| posDif e.clientPos state.pos
                    in
                    ( { model
                        | gears = Dict.update state.target (Maybe.map move) model.gears
                        , click = Just { state | drag = True, pos = e.clientPos }
                      }
                    , Cmd.none
                    )

        EndClick ->
            case model.click of
                Nothing ->
                    ( model, Debug.log "No click to end" Cmd.none )

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
                                case Dict.get target model.gears of
                                    Nothing ->
                                        ( newM, Debug.log ("No gear to play for id " ++ idToString target) Cmd.none )

                                    Just g ->
                                        let
                                            unstop gear =
                                                { gear | stopped = False }
                                        in
                                        ( { newM | gears = Dict.insert target (unstop g) model.gears }
                                        , toEngine <| engineEncoder { id = target, gear = Just g, action = PlayPause }
                                        )

                            Edit ->
                                ( { newM | details = Just target }, Cmd.none )

                            Link ->
                                ( newM, Cmd.none )

        AbortClick ->
            ( { model | click = Nothing }, Cmd.none )

        StopGear id ->
            let
                stop gear =
                    { gear | stopped = True }
            in
            ( { model | gears = Dict.update id (Maybe.map stop) model.gears }
            , toEngine <| engineEncoder { action = StopReset, id = id, gear = Nothing }
            )

        DeleteGear id ->
            ( { model | gears = Dict.remove id model.gears }, Cmd.none )

        UpdateViewPos vp ->
            ( { model | viewPos = vp }, Cmd.none )

        SVGSize res ->
            case res of
                Result.Err e ->
                    ( { model | debug = D.errorToString e }, Cmd.none )

                Result.Ok s ->
                    ( { model | svgSize = s }, Cmd.none )

        NOOP ->
            ( model, Cmd.none )

        Problem str ->
            ( { model | debug = str }, Cmd.none )



-- SUBS


subs { click } =
    Sub.batch <|
        [ soundLoaded SoundLoaded
        , newSVGSize (sizeDecoder >> SVGSize)
        ]
            ++ clickSubs click


clickSubs : Maybe ClickState -> List (Sub Msg)
clickSubs click =
    case click of
        Nothing ->
            []

        Just state ->
            [ BE.onMouseUp <| D.succeed <| EndClick
            , BE.onVisibilityChange
                (\v ->
                    Debug.log (Debug.toString v) <|
                        case v of
                            BE.Hidden ->
                                AbortClick

                            _ ->
                                NOOP
                )
            ]


dragSpaceEvents : Maybe ClickState -> List (Html.Attribute Msg)
dragSpaceEvents click =
    case click of
        Nothing ->
            []

        Just _ ->
            [ Mouse.onMove ClickMove ]


hoverEvents : Bool -> Id -> List (Html.Attribute Msg)
hoverEvents hover id =
    [ Mouse.onEnter <| always <| HoverIn id ]
        ++ (if hover then
                [ Mouse.onLeave <| always HoverOut ]

            else
                []
           )


draggableEvents id =
    [ Mouse.onDown <| StartClick id ]



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
                        { onPress = Just UpdateSoundList
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
                    [ Input.radioRow []
                        { onChange = ChangeTool
                        , options =
                            [ Input.option Play <| text "Jouer"
                            , Input.option Edit <| text "Éditer"
                            , Input.option Link <| text "Lier"
                            ]
                        , selected = Just model.tool
                        , label = Input.labelHidden "Outils"
                        }
                    , el [ width fill, height fill ] <|
                        Element.html <|
                            S.svg
                                ([ Html.Attributes.id "svg"
                                 , SS.attribute "width" "100%"
                                 , SS.attribute "height" "100%"
                                 , SA.preserveAspectRatio TypedSvg.Types.AlignNone TypedSvg.Types.Meet
                                 , computeViewBox model
                                 ]
                                    ++ dragSpaceEvents model.click
                                )
                            <|
                                List.map (viewGear model) <|
                                    Dict.toList model.gears
                    ]
                 ]
                    ++ (case model.details of
                            Nothing ->
                                []

                            Just id ->
                                case Dict.get id model.gears of
                                    Nothing ->
                                        []

                                    Just g ->
                                        [ viewDetails id g ]
                       )
                )
        ]
    }


soundView : SoundFile -> Element Msg
soundView soundFile =
    el
        [ onClick (CreateGear soundFile) ]
        (text (sFtoString soundFile))


viewGear : { a | hover : Maybe Id, click : Maybe ClickState } -> ( Id, Gear ) -> SS.Svg Msg
viewGear model ( id, g ) =
    let
        tickH =
            g.length / 15

        tickW =
            g.length / 30

        stopSize =
            g.length / 10

        stopSpace =
            g.length / 30

        interact =
            howInteract id model
    in
    S.g [ SA.transform [ Translate g.x g.y ] ]
        ([ S.g [ Html.Attributes.id <| idToString id ]
            [ S.circle
                ([ SA.cx <| Num 0
                 , SA.cy <| Num 0
                 , SA.r <| Num (g.length / 2)
                 ]
                    ++ hoverEvents (interact == Hover) id
                    ++ draggableEvents id
                )
                []
            , S.rect
                [ SA.width <| Num tickW
                , SA.height <| Num tickH
                , SA.x <| Num (tickW / -2)
                , SA.y <| Num ((g.length / -2) - tickH)
                ]
                []
            , S.rect
                [ SA.width <| Num tickW
                , SA.height <| Num tickH
                , SA.x <| Num (tickW / -2)
                , SA.y <| Num (tickH / -2)
                , SA.fill <| TypedSvg.Types.Fill Color.orange
                , SA.transform [ Rotate (g.startPercent * 360) 0 0, Translate 0 ((g.length / -2) + (tickH / 2)) ]
                ]
                []
            ]
         ]
            ++ (if g.stopped then
                    []

                else
                    [ S.rect
                        [ SA.x <| Num (stopSize / -2)
                        , SA.y <| Num ((g.length / -2) - stopSize - stopSpace)
                        , SA.width <| Num stopSize
                        , SA.height <| Num stopSize
                        , Mouse.onClick <| always <| StopGear id
                        ]
                        []
                    ]
               )
        )


viewDetails : Id -> Gear -> Element Msg
viewDetails id g =
    column [] [ Input.button [] { onPress = Just <| DeleteGear id, label = text "Supprimer" } ]


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
                viewPos.cx - w / 2

            y =
                viewPos.cy - h / 2
        in
        if landscapeOrientation then
            SA.viewBox x y w h

        else
            SA.viewBox y x h w



-- HTTP


fetchSoundList : Url.Url -> Cmd Msg
fetchSoundList url =
    Http.get
        { url = Url.toString { url | path = "/soundList" }
        , expect = Http.expectString NewSoundList
        }



-- MISC


posDif ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )
--}
