module Data.Gear exposing (..)

import Harmony as Harmo exposing (Harmony)
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Motor exposing (Motor)
import Tools.Coll as Coll exposing (Id)
import Tools.Fraction as Fract exposing (Fraction)


type alias Gear item =
    { pos : Vec2, motor : Motor, harmony : Harmony, wheel : item }


typeString : String
typeString =
    "gear"


default : w -> Gear w
default w =
    { pos = vec2 0 0, harmony = Harmo.default, motor = Motor.default, wheel = w }


toUID : Id (Gear w) -> String
toUID id =
    typeString ++ "-" ++ Coll.idToString id


encoder : (w -> List ( String, E.Value )) -> Gear w -> E.Value
encoder wEncoder g =
    E.object <|
        [ ( "pos"
          , E.object
                [ ( "x", E.float <| Vec.getX g.pos )
                , ( "y", E.float <| Vec.getY g.pos )
                ]
          )
        , ( "motors", Motor.encoder g.motor )
        ]
            ++ Harmo.encoder g.harmony
            ++ wEncoder g.wheel


decoder : D.Decoder w -> (w -> Float) -> D.Decoder (Gear w)
decoder wDecoder wheelToContentLength =
    wDecoder
        |> D.andThen
            (\w ->
                Harmo.decoder (wheelToContentLength w)
                    |> D.andThen
                        (\harmo ->
                            Field.require "motors" Motor.decoder <|
                                \motor ->
                                    Field.requireAt [ "pos", "x" ] D.float <|
                                        \px ->
                                            Field.requireAt [ "pos", "y" ] D.float <|
                                                \py ->
                                                    D.succeed
                                                        { motor = motor
                                                        , pos = vec2 px py
                                                        , harmony = harmo
                                                        , wheel = w
                                                        }
                        )
            )


type Msg
    = NewPos Vec2
    | ResizeFract Fraction


update : Msg -> Gear w -> Gear w
update msg g =
    case msg of
        NewPos p ->
            { g | pos = p }

        ResizeFract f ->
            Harmo.setFract (Fract.multiplication (Harmo.getFract g) f) g
