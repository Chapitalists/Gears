module Data.Content exposing (..)

import Coll exposing (Coll, Id)
import Data.Gear as Gear exposing (Gear)
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Sound exposing (Sound)


type Content item
    = M (Mobile item)
    | C (Collar item)
    | S Sound


encoder : (item -> List ( String, E.Value )) -> Content item -> ( String, E.Value )
encoder wheelEncoder content =
    case content of
        S s ->
            ( "sound", Sound.encoder s )

        M m ->
            ( "mobile", mobileEncoder wheelEncoder m )

        C c ->
            ( "collar", collarEncoder wheelEncoder c )


decoder : D.Decoder item -> (item -> Float) -> item -> D.Decoder (Content item)
decoder wheelDecoder wheelToCententLength defaultWheel =
    D.oneOf
        [ Field.require "sound" Sound.decoder <| \sound -> D.succeed <| S sound
        , Field.require "mobile" (mobileDecoder wheelDecoder wheelToCententLength defaultWheel) <| \mobile -> D.succeed <| M mobile
        , Field.require "collar" (collarDecoder wheelDecoder) <| \collar -> D.succeed <| C collar
        ]


type alias Mobile item =
    { motor : Id (Gear item), gears : Coll (Gear item) }


getGear : Id (Gear item) -> Mobile item -> Gear item
getGear id m =
    Coll.get id m.gears


updateGear : Id (Gear item) -> (Gear item -> Gear item) -> Mobile item -> Mobile item
updateGear id f m =
    { m | gears = Coll.update id f m.gears }


mobileEncoder : (item -> List ( String, E.Value )) -> Mobile item -> E.Value
mobileEncoder wheelEncoder m =
    E.object
        [ ( "motor", Coll.idEncoder m.motor )
        , ( "gears"
          , Coll.encoder m.gears <| Gear.encoder wheelEncoder
          )
        ]


mobileDecoder : D.Decoder item -> (item -> Float) -> item -> D.Decoder (Mobile item)
mobileDecoder wheelDecoder wheelToContentLength defaultWheel =
    D.succeed Mobile
        |> required "motor" Coll.idDecoder
        |> required "gears"
            (Coll.decoder
                (Gear.decoder wheelDecoder wheelToContentLength)
                Gear.typeString
             <|
                Gear.default defaultWheel
            )


beadUIDExtension : String -> Int -> String
beadUIDExtension parentUid i =
    parentUid ++ "-" ++ String.fromInt i


type alias Bead item =
    { length : Float, wheel : item }


beadEncoder : (item -> List ( String, E.Value )) -> Bead item -> E.Value
beadEncoder wheelEncoder b =
    E.object <| ( "length", E.float b.length ) :: wheelEncoder b.wheel


beadDecoder : D.Decoder item -> D.Decoder (Bead item)
beadDecoder wheelDecoder =
    wheelDecoder
        |> D.andThen
            (\w ->
                Field.require "length" D.float <|
                    \l ->
                        D.succeed { length = l, wheel = w }
            )


type alias Collar item =
    { matrice : Int -- nth bead to include in collar size (start to nth’s end)
    , loop : Float -- start point of loop in percent of full collar (then loops to end)
    , head : Bead item
    , beads : List (Bead item)

    -- WARNING second source of truth, just a shortcut to sounds internals
    , oneSound : Maybe { path : String, start : Float, end : Float, divs : List Float }
    }



-- TODO doesn’t updates collar.loop…


setCollarLoop :
    (( Maybe Float, Maybe Float ) -> Bead item -> Bead item)
    -> ( Maybe Float, Maybe Float )
    -> Collar item
    -> Collar item
setCollarLoop chgBeadLoop mayPoints c =
    case c.oneSound of
        Nothing ->
            c

        Just one ->
            case mayPoints of
                ( Nothing, Just end ) ->
                    let
                        safeEnd =
                            clamp one.start 1 end

                        newDivs =
                            List.filter (\x -> x < safeEnd) one.divs
                    in
                    if List.isEmpty newDivs then
                        { c | oneSound = Nothing, matrice = 1, beads = [], head = chgBeadLoop mayPoints c.head }

                    else
                        let
                            nBeads =
                                List.length newDivs

                            tmpBeads =
                                List.take nBeads c.beads

                            newBeads =
                                List.indexedMap
                                    (\i ->
                                        if i == nBeads - 1 then
                                            chgBeadLoop mayPoints

                                        else
                                            identity
                                    )
                                    tmpBeads
                        in
                        { c
                            | matrice = nBeads + 1
                            , beads = newBeads
                            , oneSound = Just { one | end = safeEnd, divs = newDivs }
                        }

                ( Just start, Nothing ) ->
                    let
                        safeStart =
                            clamp 0 one.end start

                        newDivs =
                            List.filter (\x -> x > safeStart) one.divs
                    in
                    if List.isEmpty newDivs then
                        { c
                            | oneSound = Nothing
                            , matrice = 1
                            , beads = []
                            , head = chgBeadLoop mayPoints <| getBead (List.length c.beads) c
                        }

                    else
                        let
                            nBeads =
                                List.length newDivs + 1

                            allBeads =
                                getBeads c

                            tmpBeads =
                                List.drop (List.length allBeads - nBeads) allBeads
                        in
                        case tmpBeads of
                            head :: newBeads ->
                                { c
                                    | matrice = nBeads
                                    , beads = newBeads
                                    , head = chgBeadLoop mayPoints head
                                    , oneSound = Just { one | start = safeStart, divs = newDivs }
                                }

                            [] ->
                                let
                                    _ =
                                        Debug.log "Incoherency between oneSound and beads" ( c.oneSound, c.beads )
                                in
                                c

                ( Just start, Just end ) ->
                    let
                        safeStart =
                            clamp 0 1 start

                        safeEnd =
                            clamp safeStart 1 end

                        newDivs =
                            List.map
                                (\d ->
                                    safeStart
                                        + (d - one.start)
                                        * (safeEnd - safeStart)
                                        / (one.end - one.start)
                                )
                                one.divs
                    in
                    { c
                        | oneSound = Just { one | start = safeStart, end = safeEnd, divs = newDivs }
                        , head = chgBeadLoop ( Just safeStart, List.head newDivs ) c.head
                        , beads = List.map3 (\b s e -> chgBeadLoop ( Just s, Just e ) b) c.beads newDivs <| List.drop 1 newDivs ++ [ safeEnd ]
                    }

                _ ->
                    c


setCollarDiv :
    (( Maybe Float, Maybe Float ) -> Bead item -> Bead item)
    -> Int
    -> Float
    -> Collar item
    -> Collar item
setCollarDiv chgBeadLoop i percent c =
    case c.oneSound of
        Nothing ->
            c

        Just one ->
            case List.head <| List.drop i one.divs of
                Nothing ->
                    c

                Just oldDiv ->
                    if percent <= one.start then
                        setCollarLoop chgBeadLoop ( Just percent, Nothing ) c

                    else if percent >= one.end then
                        setCollarLoop chgBeadLoop ( Nothing, Just percent ) c

                    else
                        let
                            tmpDivs =
                                List.filter (\x -> (x < percent && x < oldDiv) || (x > percent && x > oldDiv)) one.divs

                            ( preDiv, postDiv ) =
                                List.partition (\x -> x < percent) tmpDivs

                            newDivs =
                                preDiv ++ percent :: postDiv

                            oldBeads =
                                getBeads c

                            preBead =
                                List.take (List.length preDiv) oldBeads

                            mayLeftBead =
                                List.head <| List.drop (List.length preDiv) oldBeads

                            nToPost =
                                List.length oldBeads - List.length postDiv

                            mayRightBead =
                                List.head <| List.drop (nToPost - 1) oldBeads

                            postBead =
                                List.drop nToPost oldBeads
                        in
                        case ( mayLeftBead, mayRightBead ) of
                            ( Just leftBead, Just rightBead ) ->
                                let
                                    tmpBeads =
                                        preBead
                                            ++ [ chgBeadLoop ( Nothing, Just percent ) leftBead ]
                                            ++ [ chgBeadLoop ( Just percent, Nothing ) rightBead ]
                                            ++ postBead
                                in
                                case tmpBeads of
                                    newHead :: newBeads ->
                                        { c
                                            | matrice = List.length tmpBeads
                                            , head = newHead
                                            , beads = newBeads
                                            , oneSound = Just { one | divs = newDivs }
                                        }

                                    [] ->
                                        let
                                            _ =
                                                Debug.log "Incoherency1 between oneSound and beads" ( c.oneSound, c.beads )
                                        in
                                        c

                            _ ->
                                let
                                    _ =
                                        Debug.log "Incoherency2 between oneSound and beads" ( c.oneSound, c.beads )
                                in
                                c


getBeads : Collar item -> List (Bead item)
getBeads c =
    c.head :: c.beads



-- TODO if i >= length, get differs from update, bug, see Common.deleteWheel, updates the got bead


getBead : Int -> Collar item -> Bead item
getBead i c =
    case List.head <| List.drop i <| getBeads c of
        Just b ->
            b

        Nothing ->
            let
                _ =
                    Debug.log ("Cannot get Bead " ++ String.fromInt i) ( i, c )
            in
            c.head


updateBead : Int -> (Bead item -> Bead item) -> Collar item -> Collar item
updateBead =
    updateBeadHelper False


updateBeadKeepOneSound : Int -> (Bead item -> Bead item) -> Collar item -> Collar item
updateBeadKeepOneSound =
    updateBeadHelper True


updateBeadHelper : Bool -> Int -> (Bead item -> Bead item) -> Collar item -> Collar item
updateBeadHelper keepOneSound i f collar =
    let
        c =
            if keepOneSound then
                collar

            else
                { collar | oneSound = Nothing }
    in
    if i <= 0 then
        { c | head = f c.head }

    else
        { c
            | beads =
                List.concat
                    [ List.take (i - 1) c.beads
                    , case List.head <| List.drop (i - 1) c.beads of
                        Nothing ->
                            []

                        Just b ->
                            [ f b ]
                    , List.drop i c.beads
                    ]
        }


getCumulLengthAt : Int -> Collar item -> Float
getCumulLengthAt i c =
    List.foldl (\b sum -> sum + b.length) 0 <| List.take i <| getBeads c


getMatriceLength : Collar item -> Float
getMatriceLength c =
    getCumulLengthAt c.matrice c


collarEncoder : (item -> List ( String, E.Value )) -> Collar item -> E.Value
collarEncoder wheelEncoder c =
    E.object <|
        [ ( "matriceSize", E.int c.matrice )
        , ( "loopStart", E.float c.loop )
        , ( "beads"
          , E.list (beadEncoder wheelEncoder) <| getBeads c
          )
        ]
            ++ (Maybe.withDefault [] <|
                    Maybe.map
                        (\oneSound ->
                            [ ( "oneSoundName", E.string oneSound.path )
                            , ( "divs", E.list E.float oneSound.divs )
                            , ( "start", E.float oneSound.start )
                            , ( "end", E.float oneSound.end )
                            ]
                        )
                        c.oneSound
               )


collarDecoder : D.Decoder item -> D.Decoder (Collar item)
collarDecoder wheelDecoder =
    Field.attempt "matriceSize" D.int <|
        \mayMatrice ->
            Field.require "loopStart" D.float <|
                \loop ->
                    Field.require "beads" (D.list <| beadDecoder wheelDecoder) <|
                        \beads ->
                            Field.attempt "oneSoundName" D.string <|
                                \oneSoundStr ->
                                    Field.attempt "divs" (D.list D.float) <|
                                        \oneSoundDivs ->
                                            Field.attempt "start" D.float <|
                                                \oneSoundStart ->
                                                    Field.attempt "end" D.float <|
                                                        \oneSoundEnd ->
                                                            let
                                                                matrice =
                                                                    Maybe.withDefault (List.length beads) mayMatrice
                                                            in
                                                            case beads of
                                                                head :: list ->
                                                                    D.succeed
                                                                        { matrice = matrice
                                                                        , loop = loop
                                                                        , head = head
                                                                        , beads = list
                                                                        , oneSound =
                                                                            Maybe.map4
                                                                                (\str start end divs ->
                                                                                    { path = str
                                                                                    , start = start
                                                                                    , end = end
                                                                                    , divs = divs
                                                                                    }
                                                                                )
                                                                                oneSoundStr
                                                                                oneSoundStart
                                                                                oneSoundEnd
                                                                                oneSoundDivs
                                                                        }

                                                                _ ->
                                                                    D.fail "Collar should have at least one bead"
