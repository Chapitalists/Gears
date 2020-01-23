module Data.Gear exposing (..)

import Coll exposing (Coll, Id)
import Fraction as Fract exposing (Fraction)
import Harmony as Harmo exposing (Harmony)
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Link exposing (DrawLink, Link)
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Motor exposing (Motor)


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


copy : Id (Gear w) -> Coll (Gear w) -> Coll (Gear w)
copy id coll =
    let
        g =
            Coll.get id coll

        base =
            Coll.idMap <| Maybe.withDefault id <| Harmo.getBaseId g.harmony

        newG =
            { g
                | pos = Vec.add g.pos (vec2 (Harmo.getLength g.harmony coll * 1.1) 0)
                , harmony = { fract = g.harmony.fract, ref = Harmo.Other <| Coll.idMap base } -- TODO abuses harmo internals
                , motor = []
            }

        ( newId, newColl ) =
            Coll.insertTellId newG coll

        newLink =
            Link.map ( id, newId )
    in
    Coll.update base (Harmo.insert newId >> Harmo.addLink newLink) newColl


toDrawLink : Coll (Gear w) -> Link (Gear w) -> DrawLink
toDrawLink coll l =
    let
        get id =
            Coll.get id coll

        toCircle g =
            { c = g.pos, d = Harmo.getLength g.harmony coll }

        f =
            get >> toCircle
    in
    Tuple.mapBoth f f l


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


decoder : D.Decoder w -> D.Decoder (Gear w)
decoder wDecoder =
    wDecoder
        |> D.andThen
            (\w ->
                Harmo.decoder
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
