module Harmony exposing (..)

import Coll exposing (Coll, Id)
import Fraction as Fract exposing (Fraction)
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Link exposing (Link)
import Round



-- TODO Keep Ref internal
-- TODO Debug.log reveals impossible cases, maybe make fns with ’em intern and expose a safe version, higher level


type alias Harmony =
    { fract : Fraction, ref : Ref }


type alias Harmonized g =
    { g | harmony : Harmony }


default : Harmony
default =
    { fract = Fract.integer 0, ref = defaultRef }


getFract : Harmonized g -> Fraction
getFract g =
    g.harmony.fract


setFract : Fraction -> Harmonized g -> Harmonized g
setFract fract g =
    let
        harmo =
            g.harmony
    in
    { g | harmony = { harmo | fract = fract } }


type Ref
    = Other (Id Harmony)
    | Self
        { unit : Float

        -- TODO better be a Set than a List, either deOpacify Id or add Set in Coll lib
        , group : List (Id Harmony)
        , links : List (Link Harmony)
        }


view : Id (Harmonized g) -> Coll (Harmonized g) -> (Id (Harmonized g) -> String) -> String
view id coll getName =
    let
        harmo =
            getHarmo id coll
    in
    Fract.toString harmo.fract
        ++ " de "
        ++ (case harmo.ref of
                Self r ->
                    Round.round 2 r.unit

                Other rId ->
                    case (Coll.get (Coll.idMap rId) coll).harmony.ref of
                        Self r ->
                            Round.round 2 r.unit ++ " ( " ++ (getName <| Coll.idMap rId) ++ " )"

                        Other _ ->
                            Debug.log "IMPOSSIBLE Other refer to another Other" "BUG Harmo.view"
           )


defaultRef : Ref
defaultRef =
    Self { unit = 0, group = [], links = [] }


clean : Id (Harmonized g) -> Coll (Harmonized g) -> Coll (Harmonized g)
clean id coll =
    case (getHarmo id coll).ref of
        Other rId ->
            Coll.update (Coll.idMap rId) (remove id) coll

        Self r ->
            Debug.todo "Clean Base"


changeSelf : Id (Harmonized g) -> Float -> Coll (Harmonized g) -> Coll (Harmonized g)
changeSelf id length coll =
    let
        g =
            Coll.get id coll

        harmo =
            g.harmony
    in
    case harmo.ref of
        Self r ->
            Coll.update id (always { g | harmony = { harmo | ref = Self { r | unit = length } } }) coll

        Other rId ->
            coll
                |> Coll.update id (always { g | harmony = newSelf length })
                |> Coll.update (Coll.idMap rId) (remove id)


resizeFree : Id (Harmonized g) -> Float -> Coll (Harmonized g) -> Coll (Harmonized g)
resizeFree id length coll =
    changeSelf id (length / Fract.toFloat (getHarmo id coll).fract) coll


getLengthId : Id (Harmonized g) -> Coll (Harmonized g) -> Float
getLengthId id coll =
    getLength (getHarmo id coll) coll


getLength : Harmony -> Coll (Harmonized g) -> Float
getLength harmo coll =
    case harmo.ref of
        Self { unit } ->
            unit * Fract.toFloat harmo.fract

        Other id ->
            let
                { ref } =
                    (Coll.get (Coll.idMap id) coll).harmony
            in
            case ref of
                Self { unit } ->
                    unit * Fract.toFloat harmo.fract

                Other _ ->
                    Debug.log "IMPOSSIBLE Ref isn’t a base" 0


newSelf : Float -> Harmony
newSelf length =
    { fract = Fract.integer 1, ref = Self { unit = length, group = [], links = [] } }


hasHarmonics : Harmony -> Bool
hasHarmonics h =
    case h.ref of
        Self { group } ->
            if List.isEmpty group then
                False

            else
                True

        Other _ ->
            False


getHarmonicGroup : Id (Harmonized g) -> Coll (Harmonized g) -> List (Id (Harmonized g))
getHarmonicGroup id coll =
    case (getHarmo id coll).ref of
        Self { group } ->
            id :: List.map Coll.idMap group

        Other _ ->
            [ id ]


getBaseId : Harmony -> Maybe (Id (Harmonized g))
getBaseId h =
    case h.ref of
        Self _ ->
            Nothing

        Other id ->
            Just <| Coll.idMap id


insert : Id (Harmonized g) -> Harmonized g -> Harmonized g
insert id g =
    let
        harmo =
            g.harmony
    in
    case harmo.ref of
        Other _ ->
            Debug.log "Can’t add to ref group if not base" g

        Self r ->
            { g | harmony = { harmo | ref = Self { r | group = Coll.idMap id :: r.group } } }


remove : Id (Harmonized g) -> Harmonized g -> Harmonized g
remove id g =
    let
        harmo =
            g.harmony
    in
    case harmo.ref of
        Other _ ->
            Debug.log "Can’t remove from ref group if not base" g

        Self r ->
            let
                isGoodLink l =
                    Tuple.first l /= Coll.idMap id && Tuple.second l /= Coll.idMap id
            in
            { g
                | harmony =
                    { harmo
                        | ref =
                            Self
                                { r
                                    | group = List.filter ((/=) <| Coll.idMap id) r.group
                                    , links = List.filter isGoodLink r.links
                                }
                    }
            }


addLink : Link (Harmonized g) -> Harmonized g -> Harmonized g
addLink l g =
    let
        harmo =
            g.harmony
    in
    case harmo.ref of
        Other _ ->
            Debug.log "Can’t add link if not base" g

        Self r ->
            { g | harmony = { harmo | ref = Self { r | links = Link.map l :: r.links } } }


isActiveLink : Link (Harmonized g) -> Harmony -> Bool
isActiveLink l h =
    case h.ref of
        Other _ ->
            Debug.log "Can’t check active links if not base" False

        Self { links } ->
            List.any (Link.equal <| Link.map l) links


getLinks : Harmony -> List (Link (Harmonized g))
getLinks h =
    case h.ref of
        Other _ ->
            []

        Self { links } ->
            List.map Link.map links


getHarmo : Id (Harmonized g) -> Coll (Harmonized g) -> Harmony
getHarmo id coll =
    (Coll.get id coll).harmony


encoder : Harmony -> List ( String, E.Value )
encoder h =
    [ ( "ref", refEncoder h.ref )
    , ( "fract", Fract.encoder h.fract )
    ]


decoder : D.Decoder Harmony
decoder =
    Field.require "ref" refDecoder <|
        \ref ->
            Field.require "fract" Fract.decoder <|
                \fract ->
                    D.succeed
                        { ref = ref
                        , fract = fract
                        }


refEncoder : Ref -> E.Value
refEncoder ref =
    case ref of
        Other id ->
            E.object [ ( "other", Coll.idEncoder id ) ]

        Self r ->
            E.object <|
                [ ( "unit", E.float r.unit )
                , ( "group", E.list Coll.idEncoder r.group )
                , ( "links", E.list Link.encoder r.links )
                ]


refDecoder : D.Decoder Ref
refDecoder =
    Field.attempt "other" Coll.idDecoder <|
        \mayId ->
            case mayId of
                Just id ->
                    D.succeed <| Other id

                Nothing ->
                    Field.require "unit" D.float <|
                        \unit ->
                            Field.require "group" (D.list Coll.idDecoder) <|
                                \group ->
                                    Field.require "links" (D.list Link.decoder) <|
                                        \links ->
                                            D.succeed <|
                                                Self
                                                    { unit = unit
                                                    , group = group
                                                    , links = links
                                                    }
