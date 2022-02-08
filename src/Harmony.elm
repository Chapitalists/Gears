module Harmony exposing (..)

import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Link exposing (Link)
import Round
import Tools.Coll as Coll exposing (Coll, Id)
import Tools.Fraction as Fract exposing (Fraction)



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
    = Rate Float -- Duration / Content length
    | Duration Float


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
                        Duration d ->
                            " de " ++ Round.round 2 d

                        Rate r ->
                            " du contenu x" ++ Round.round 2 (1 / r)

                Other rId ->
                    let
                        name =
                            getName <| Coll.idMap rId
                    in
                    case (Coll.get (Coll.idMap rId) coll).harmony.ref of
                        Self { unit } ->
                            case unit of
                                Duration d ->
                                    " de " ++ Round.round 2 d ++ " ( " ++ name ++ " )"

                                Rate r ->
                                    " du contenu de " ++ name ++ " x" ++ Round.round 2 (1 / r)

                        Other _ ->
                            Debug.log "IMPOSSIBLE Other refer to another Other" "BUG Harmo.view"
           )


defaultRef : Ref
defaultRef =
    Self { unit = Rate 1, group = [], links = [] }


clean : Id (Harmonized g) -> Coll (Harmonized g) -> Coll (Harmonized g)
clean id coll =
    case (getHarmo id coll).ref of
        Other rId ->
            Coll.update (Coll.idMap rId) (remove id) coll

        Self r ->
            Debug.todo "Clean Base"


hardEmptySelf : Id (Harmonized g) -> Coll (Harmonized g) -> Coll (Harmonized g)
hardEmptySelf id coll =
    let
        harmo =
            getHarmo id coll
    in
    case harmo.ref of
        Self { unit } ->
            Coll.update id (\g -> { g | harmony = { harmo | ref = Self { unit = unit, group = [], links = [] } } }) coll

        _ ->
            coll


changeSelfUnit : Id (Harmonized g) -> SelfUnit -> Coll (Harmonized g) -> Coll (Harmonized g)
changeSelfUnit id su coll =
    let
        g =
            Coll.get id coll

        harmo =
            g.harmony
    in
    case harmo.ref of
        Self r ->
            Coll.update id (always { g | harmony = { harmo | ref = Self { r | unit = su } } }) coll

        Other rId ->
            coll
                |> Coll.update id (always { g | harmony = newHarmoWithSelfUnit su })
                |> Coll.update (Coll.idMap rId) (remove id)


changeRate : Id (Harmonized g) -> Float -> Float -> Coll (Harmonized g) -> Coll (Harmonized g)
changeRate id newDur contentLength =
    changeSelfUnit id <| Rate (newDur / contentLength)


toRate : (Harmonized g -> Float) -> Id (Harmonized g) -> Coll (Harmonized g) -> Coll (Harmonized g)
toRate getContentLength id coll =
    let
        g =
            Coll.get id coll
    in
    changeSelfUnit id (Rate (getLength getContentLength g coll / getContentLength g)) coll


changeDuration : Id (Harmonized g) -> Float -> Coll (Harmonized g) -> Coll (Harmonized g)
changeDuration id newDur =
    changeSelfUnit id <| Duration newDur


changeContentKeepLength : Id (Harmonized g) -> Float -> Float -> Coll (Harmonized g) -> Coll (Harmonized g)
changeContentKeepLength id newContentLength oldContentLength coll =
    case (getHarmo id coll).ref of
        Self { unit } ->
            case unit of
                Rate r ->
                    changeSelfUnit id (Rate <| r * oldContentLength / newContentLength) coll

                _ ->
                    coll

        _ ->
            coll


resizeFree : Id (Harmonized g) -> Float -> Float -> Coll (Harmonized g) -> Coll (Harmonized g)
resizeFree id length contentLength coll =
    changeRate id (length / Fract.toFloat (getHarmo id coll).fract) contentLength coll


toContentLength : Id (Harmonized g) -> Coll (Harmonized g) -> Coll (Harmonized g)
toContentLength id =
    changeSelfUnit id <| Rate 1


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
        |> Coll.update baseId (insert newId >> addLink link)


getLengthId : (Harmonized g -> Float) -> Id (Harmonized g) -> Coll (Harmonized g) -> Float
getLengthId getContentLength id coll =
    getLength getContentLength (Coll.get id coll) coll


getLength : (Harmonized g -> Float) -> Harmonized g -> Coll (Harmonized g) -> Float
getLength getContentLength el coll =
    let
        harmo =
            el.harmony

        refUnit =
            case harmo.ref of
                Self { unit } ->
                    case unit of
                        Duration d ->
                            d

                        Rate r ->
                            r * getContentLength el

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
                                Duration d ->
                                    d

                                Rate r ->
                                    r * getContentLength ell

                        Other _ ->
                            let
                                _ =
                                    Debug.log "IMPOSSIBLE Ref isn’t a base" ( harmo, coll )
                            in
                            getContentLength el
    in
    Fract.toFloat harmo.fract * refUnit


newHarmoWithSelfUnit : SelfUnit -> Harmony
newHarmoWithSelfUnit su =
    { fract = Fract.integer 1, ref = Self { unit = su, group = [], links = [] } }


newDuration : Float -> Harmony
newDuration d =
    newHarmoWithSelfUnit <| Duration d


newRate : Float -> Harmony
newRate r =
    newHarmoWithSelfUnit <| Rate r


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


decoder : Float -> D.Decoder Harmony
decoder contentLength =
    Field.require "ref" (refDecoder contentLength) <|
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
        Rate r ->
            [ ( "rate", E.float r ) ]

        Duration d ->
            [ ( "duration", E.float d ) ]


selfUnitDecoder : Float -> D.Decoder SelfUnit
selfUnitDecoder contentLength =
    D.oneOf
        [ Field.require "unit" D.float <| \d -> D.succeed <| Rate (d / contentLength)
        , Field.require "duration" D.float <| \d -> D.succeed <| Duration d
        , Field.require "rate" D.float <| \r -> D.succeed <| Rate r
        ]


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


refDecoder : Float -> D.Decoder Ref
refDecoder contentLength =
    Field.attempt "other" Coll.idDecoder <|
        \mayId ->
            case mayId of
                Just id ->
                    D.succeed <| Other id

                Nothing ->
                    selfUnitDecoder contentLength
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
