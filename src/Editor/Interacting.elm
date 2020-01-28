module Editor.Interacting exposing (..)

import Coll exposing (Id)
import Data.Mobile exposing (Geer)
import Pack exposing (Packed)
import Sound exposing (Sound)


type Interactable
    = ISurface
    | IGear (Id Geer)
    | IResizeHandle (Id Geer) Bool -- True = right
    | IPack (Id Packed)
    | ISound Sound


type Zone
    = ZSurface
    | ZPack
