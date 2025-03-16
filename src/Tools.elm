module Tools exposing (..)

import Math.Vector2 exposing (Vec2)
import Utils.Panel exposing (Panel)


type alias Tools =
    { panels : List Panel
    , floating : List Vec2
    }



--type alias FloatingTool =
--    { pos : Vec2
--    , }
