port module Main exposing (..)

import Browser
import Browser.Dom as DOM
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Color
import Draggable
import Draggable.Events
import Element exposing (..)
import Element.Background as Bg
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Http
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


port play : E.Value -> Cmd msg


port soundLoaded : (String -> msg) -> Sub msg



-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        , onUrlRequest = urlReq
        , onUrlChange = urlCh
        }


urlReq _ =
    NOOP


urlCh _ =
    NOOP



-- MODEL


type alias Model =
    { connected : Bool
    , currentUrl : Url.Url
    , soundList : Set String
    , loadedSoundList : List SoundFile
    , tool : Tool
    , gears : List Gear
    , viewPos : ViewPos
    , svgSize : Size
    , nextId : Int
    , drag : Draggable.State Id
    , debug : String
    }


type alias ViewPos =
    { cx : Float, cy : Float, smallestSize : Float }


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
    , id : Id
    , sound : SoundFile
    }


type alias Id =
    String


encodeGear : Gear -> E.Value
encodeGear g =
    E.object
        [ ( "type", E.string "gear" )
        , ( "soundName", E.string <| sFtoString g.sound )
        , ( "gearId", E.string g.id )
        ]


type alias Size =
    { width : Float
    , height : Float
    }


cmdSVGSize =
    Task.attempt checkSVGSize <| DOM.getElement "svg"


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    ( Model False url Set.empty [] Play [] (ViewPos 0 0 10) (Size 0 0) 1 Draggable.init ""
    , Cmd.batch [ cmdSVGSize, fetchSoundList url ]
    )



-- UPDATE


type Msg
    = NewSoundList (Result Http.Error String)
    | UpdateSoundList
    | ChangeTool Tool
    | RequestSoundLoad String
    | RequestPlay Playable
    | SoundLoaded String
    | CreateGear SoundFile
    | UpdateViewPos (Maybe ViewPos)
    | SVGSize Size
    | GearDrag Draggable.Delta
    | GearClick Id
    | NOOP
    | Problem String
    | DragMsg (Draggable.Msg Id)


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
            ( model
            , if Set.member n model.soundList then
                loadSound n

              else
                Cmd.none
            )

        RequestPlay _ ->
            ( model, Cmd.none )

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
                | gears = Gear length x y 0.2 ("gear-" ++ String.fromInt model.nextId) sf :: model.gears
                , nextId = model.nextId + 1
                , viewPos = { cx = x, cy = y, smallestSize = length * 2 * 4 }
              }
            , Cmd.none
            )

        GearDrag ( dx, dy ) ->
            case model.tool of
                Play ->
                    let
                        scale =
                            model.viewPos.smallestSize / min model.svgSize.height model.svgSize.width
                    in
                    ( { model | gears = List.map (\g -> { g | x = g.x + dx * scale, y = g.y + dy * scale }) model.gears }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GearClick id ->
            case List.head <| List.filter (\g -> g.id == id) model.gears of
                Just g ->
                    ( model, play <| encodeGear g )

                Nothing ->
                    ( { model | debug = "Not found clicked id : " ++ id }, Cmd.none )

        UpdateViewPos maybeVP ->
            case maybeVP of
                Just vp ->
                    ( { model | viewPos = vp }, cmdSVGSize )

                Nothing ->
                    ( model, cmdSVGSize )

        SVGSize s ->
            ( { model | svgSize = s }, Cmd.none )

        NOOP ->
            ( model, Cmd.none )

        Problem str ->
            ( { model | debug = str }, Cmd.none )

        DragMsg dm ->
            Draggable.update
                (Draggable.customConfig
                    [ Draggable.Events.onDragBy GearDrag
                    , Draggable.Events.onClick GearClick
                    ]
                )
                dm
                model



-- SUBS


subs { drag } =
    Sub.batch
        [ soundLoaded SoundLoaded
        , onResize (\_ _ -> UpdateViewPos Nothing)
        , Draggable.subscriptions DragMsg drag
        ]



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
                [ column [ height fill, Bg.color (rgb 0.5 0.5 0.5), Font.color (rgb 1 1 1), spacing 20, padding 10 ]
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
                , column []
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
                    , Element.html <|
                        S.svg
                            [ Html.Attributes.id "svg"
                            , SS.attribute "width" "100vw"
                            , SS.attribute "height" "100vh"
                            , SA.display TypedSvg.Types.DisplayBlock
                            , SA.preserveAspectRatio TypedSvg.Types.AlignNone TypedSvg.Types.Meet
                            , computeViewBox model
                            ]
                        <|
                            List.map viewGear model.gears
                    ]
                ]
        ]
    }


soundView : SoundFile -> Element Msg
soundView soundFile =
    el
        [ onClick (CreateGear soundFile) ]
        (text (sFtoString soundFile))


viewGear : Gear -> SS.Svg Msg
viewGear g =
    let
        tickH =
            g.length / 15

        tickW =
            g.length / 30
    in
    S.g [ SA.transform [ Translate g.x g.y ] ]
        [ S.g [ Html.Attributes.id g.id ]
            [ S.circle
                [ SA.cx <| Num 0
                , SA.cy <| Num 0
                , SA.r <| Num (g.length / 2)
                , Draggable.mouseTrigger g.id DragMsg
                ]
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


checkSVGSize : Result DOM.Error DOM.Element -> Msg
checkSVGSize res =
    case res of
        Err (DOM.NotFound str) ->
            Problem str

        Ok { element } ->
            SVGSize { width = element.width, height = element.height }


computeViewBox : Model -> SS.Attribute msg
computeViewBox { viewPos, svgSize } =
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
