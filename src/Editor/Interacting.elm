module Editor.Interacting exposing (..)

import Data.Common exposing (Identifier)
import Data.Mobile exposing (Geer)
import Link exposing (Link)
import Pack exposing (Packed)
import Sound exposing (Sound)
import Tools.Coll exposing (Id)


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
    | IWaveMapSel


type Zone
    = ZSurface
    | ZPack
    | ZWave
    | ZWaveMap


type WavePart
    = Mini
    | Main


type Cursor
    = LoopStart WavePart
    | LoopEnd WavePart
    | StartOffset WavePart
    | Divide Int WavePart
    | ViewStart
    | ViewEnd
