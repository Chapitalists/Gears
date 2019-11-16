module port Engine exposing (..)


import Json.Encode as E


port toEngine : E.Value -> Cmd msg

type State = S {
         playing : List (Playable)}




type Playable
    = PGear ( Id Gear, Gear )

