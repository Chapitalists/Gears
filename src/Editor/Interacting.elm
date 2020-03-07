module Editor.Interacting exposing (..)

import Coll exposing (Id)
import Data.Common exposing (Identifier)
import Data.Mobile exposing (Geer)
import Link exposing (Link)
import Pack exposing (Packed)
import Sound exposing (Sound)


type Interactable
    = ISurface
    | IPack
    | IWheel Identifier
    | IResizeHandle (Id Geer) Bool -- True = right
    | IPacked (Id Packed)
    | ILink (Link Geer)
    | ISound Sound
    | IWaveCursor Cursor
    | IWaveSel


type Zone
    = ZSurface
    | ZPack
    | ZWave


type Cursor
    = LoopStart
    | LoopEnd
    | StartOffset
