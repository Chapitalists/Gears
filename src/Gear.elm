module Gear exposing (..)

import Coll exposing (Id)
import Color
import Fraction as Fract exposing (Fraction)
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
        { refId : Id Ref
        , fract : Fraction
        , pos : Vec2
        , startPercent : Float
        , sound : Sound
        }


type Ref
    = R
        { unit : Float
        , nRefs : Int
        }


decRefWhenDeleteGear : Ref -> Ref
decRefWhenDeleteGear (R r) =
    R { r | nRefs = r.nRefs - 1 }


isUsed : Ref -> Bool
isUsed (R r) =
    r.nRefs /= 0


defaultRef =
    R { unit = 1, nRefs = 0 }


stringType : String
stringType =
    "gear"


toUID : Id Gear -> String
toUID id =
    stringType ++ "-" ++ Coll.idToString id


fromSound : Sound -> Vec2 -> Id Ref -> ( Gear, Ref )
fromSound s p refId =
    ( G
        { refId = refId
        , fract = Fract.integer 1
        , pos = p
        , startPercent = 0
        , sound = s
        }
    , R { unit = Sound.length s, nRefs = 1 }
    )


getRefId : Gear -> Id Ref
getRefId (G g) =
    g.refId


getLength : ( Gear, Ref ) -> Float
getLength ( G { fract }, R r ) =
    r.unit * Fract.toFloat fract


getPos : Gear -> Vec2
getPos (G g) =
    g.pos


encoder : ( Id Gear, Gear, Ref ) -> E.Value
encoder ( id, G g, R r ) =
    E.object
        [ ( "type", E.string stringType )
        , ( "id", E.string <| toUID id )
        , ( "length", E.float <| getLength ( G g, R r ) )
        , ( "soundName", E.string <| Sound.toString g.sound )
        ]


type Mod
    = None
    | Hovered
    | Clicked
    | Dragged


type Msg
    = Move Vec2
    | ResizeFract Fraction


update : Msg -> Gear -> Gear
update msg (G g) =
    case msg of
        Move d ->
            G { g | pos = add d g.pos }

        ResizeFract f ->
            G { g | fract = Fract.multiplication g.fract f }


type OutMsg
    = InteractMsg (Interact.Msg String)
    | GearMsg ( Id Gear, Msg )


view : ( Id Gear, Gear, Ref ) -> Mod -> Svg OutMsg
view ( id, G g, R r ) mod =
    let
        length =
            getLength ( G g, R r )

        tickH =
            length / 15

        tickW =
            length / 30
    in
    S.g [ SA.transform [ Translate (getX g.pos) (getY g.pos) ] ]
        [ S.g [ Html.Attributes.id <| toUID id ]
            [ S.circle
                ([ SA.cx <| Num 0
                 , SA.cy <| Num 0
                 , SA.r <| Num (length / 2)
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
                , SA.y <| Num ((length / -2) - tickH)
                ]
                []
            , S.rect
                [ SA.width <| Num tickW
                , SA.height <| Num tickH
                , SA.x <| Num (tickW / -2)
                , SA.y <| Num (tickH / -2)
                , SA.fill <| TypedSvg.Types.Fill Color.orange
                , SA.transform [ Rotate (g.startPercent * 360) 0 0, Translate 0 ((length / -2) + (tickH / 2)) ]
                ]
                []
            ]
        ]
