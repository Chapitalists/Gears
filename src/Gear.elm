module Gear exposing (..)

import Coll
import Color
import Html
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Interact
import Json.Encode as E
import Math.Vector2 exposing (..)
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (Length(..), Transform(..))


type Gear
    = G
        { length : Float
        , pos : Vec2
        , startPercent : Float
        , stopped : Bool
        , sound : Sound
        }


stringType : String
stringType =
    "gear"


toUID : Coll.Id Gear -> String
toUID id =
    stringType ++ Coll.idToString id


fromSound : Sound -> Vec2 -> Gear
fromSound s p =
    G
        { length = Sound.length s
        , pos = p
        , startPercent = 0
        , stopped = False
        , sound = s
        }


move : Vec2 -> Gear -> Gear
move d (G g) =
    G { g | pos = add d g.pos }


play : Gear -> Gear
play (G g) =
    G { g | stopped = True }


stop : Gear -> Gear
stop (G g) =
    G { g | stopped = False }


encoder : ( Coll.Id Gear, Gear ) -> E.Value
encoder ( id, G g ) =
    E.object
        [ ( "type", E.string stringType )
        , ( "id", E.string <| toUID id )
        , ( "soundName", E.string <| Sound.toString g.sound )
        ]


type Mod
    = None
    | Hovered
    | Clicked
    | Dragged


type Msg
    = Play
    | Stop


update : Msg -> Gear -> Gear
update msg (G g) =
    case msg of
        Play ->
            G { g | stopped = False }

        Stop ->
            G { g | stopped = True }


type OutMsg
    = InteractMsg (Interact.Msg String)
    | GearMsg ( Coll.Id Gear, Msg )


view : ( Coll.Id Gear, Gear ) -> Mod -> Svg OutMsg
view ( id, G g ) mod =
    let
        tickH =
            g.length / 15

        tickW =
            g.length / 30

        stopSize =
            g.length / 10

        stopSpace =
            g.length / 30
    in
    S.g [ SA.transform [ Translate (getX g.pos) (getY g.pos) ] ]
        ([ S.g [ Html.Attributes.id <| toUID id ]
            [ S.circle
                ([ SA.cx <| Num 0
                 , SA.cy <| Num 0
                 , SA.r <| Num (g.length / 2)
                 ]
                    ++ (List.map (Html.Attributes.map InteractMsg) <|
                            Interact.hoverEvents (mod == Hovered) (toUID id)
                                ++ Interact.draggableEvents (toUID id)
                       )
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
                        , Mouse.onClick <| always <| GearMsg ( id, Stop )
                        ]
                        []
                    ]
               )
        )
