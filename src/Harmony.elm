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
-- TODO Why Other can’t point to Other ? every fn could be recursive this way and no fallback cases
--      But there could be circular references instead…


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


type SelfUnit
    = ContentLength
    | Unit Float


type Ref
    = Other (Id Harmony)
    | Self
        { unit : SelfUnit

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
        ++ (case harmo.ref of
                Self { unit } ->
                    case unit of
                        Unit float ->
                            " de " ++ Round.round 2 float

                        ContentLength ->
                            " du contenu"

                Other rId ->
                    let
                        name =
                            getName <| Coll.idMap rId
                    in
                    case (Coll.get (Coll.idMap rId) coll).harmony.ref of
                        Self { unit } ->
                            case unit of
                                Unit float ->
                                    " de " ++ Round.round 2 float ++ " ( " ++ name ++ " )"

                                ContentLength ->
                                    " du contenu de " ++ name

                        Other _ ->
                            Debug.log "IMPOSSIBLE Other refer to another Other" "BUG Harmo.view"
           )


defaultRef : Ref
defaultRef =
    Self { unit = ContentLength, group = [], links = [] }


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
            Coll.update id (always { g | harmony = { harmo | ref = Self { r | unit = Unit length } } }) coll

        Other rId ->
            coll
                |> Coll.update id (always { g | harmony = newSelf length })
                |> Coll.update (Coll.idMap rId) (remove id)


resizeFree : Id (Harmonized g) -> Float -> Coll (Harmonized g) -> Coll (Harmonized g)
resizeFree id length coll =
    changeSelf id (length / Fract.toFloat (getHarmo id coll).fract) coll


toContentLength : Id (Harmonized g) -> Coll (Harmonized g) -> Coll (Harmonized g)
toContentLength id coll =
    case (getHarmo id coll).ref of
        Self _ ->
            Coll.update id (\g -> { g | harmony = newContentLength }) coll

        Other rId ->
            coll
                |> Coll.update id (\g -> { g | harmony = newContentLength })
                |> Coll.update (Coll.idMap rId) (remove id)


makeCopy : Id (Harmonized g) -> Id (Harmonized g) -> Coll (Harmonized g) -> Coll (Harmonized g)
makeCopy id newId coll =
    let
        harmo =
            getHarmo id coll

        baseId =
            Maybe.withDefault id <| getBaseId harmo

        newHarmo =
            { fract = harmo.fract, ref = Other <| Coll.idMap baseId }

        link =
            ( id, newId )
    in
    coll
        |> Coll.update newId (\g -> { g | harmony = newHarmo })
        |> Coll.update id (insert newId >> addLink link)


getLengthId : (Harmonized g -> Float) -> Id (Harmonized g) -> Coll (Harmonized g) -> Float
getLengthId f id coll =
    getLength f (Coll.get id coll) coll


getLength : (Harmonized g -> Float) -> Harmonized g -> Coll (Harmonized g) -> Float
getLength getContentLength el coll =
    let
        harmo =
            el.harmony

        refUnit =
            case harmo.ref of
                Self { unit } ->
                    case unit of
                        Unit float ->
                            float

                        ContentLength ->
                            getContentLength el

                Other idd ->
                    let
                        ell =
                            Coll.get (Coll.idMap idd) coll

                        { ref } =
                            ell.harmony
                    in
                    case ref of
                        Self { unit } ->
                            case unit of
                                Unit float ->
                                    float

                                ContentLength ->
                                    getContentLength ell

                        Other _ ->
                            let
                                _ =
                                    Debug.log "IMPOSSIBLE Ref isn’t a base" ( harmo, coll )
                            in
                            getContentLength el
    in
    Fract.toFloat harmo.fract * refUnit


newSelf : Float -> Harmony
newSelf length =
    { fract = Fract.integer 1, ref = Self { unit = Unit length, group = [], links = [] } }


newContentLength : Harmony
newContentLength =
    { fract = Fract.integer 1, ref = Self { unit = ContentLength, group = [], links = [] } }


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
            let
                _ =
                    Debug.log "Can’t check active links if not base" h
            in
            False

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


selfUnitEncoder : SelfUnit -> List ( String, E.Value )
selfUnitEncoder su =
    case su of
        ContentLength ->
            []

        Unit f ->
            [ ( "unit", E.float f ) ]


selfUnitDecoder : D.Decoder SelfUnit
selfUnitDecoder =
    Field.attempt "unit" D.float <|
        \mayUnit ->
            D.succeed <|
                Maybe.withDefault ContentLength <|
                    Maybe.map Unit mayUnit


refEncoder : Ref -> E.Value
refEncoder ref =
    case ref of
        Other id ->
            E.object [ ( "other", Coll.idEncoder id ) ]

        Self r ->
            E.object <|
                [ ( "group", E.list Coll.idEncoder r.group )
                , ( "links", E.list Link.encoder r.links )
                ]
                    ++ selfUnitEncoder r.unit


refDecoder : D.Decoder Ref
refDecoder =
    Field.attempt "other" Coll.idDecoder <|
        \mayId ->
            case mayId of
                Just id ->
                    D.succeed <| Other id

                Nothing ->
                    selfUnitDecoder
                        |> (D.andThen <|
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
                           )
