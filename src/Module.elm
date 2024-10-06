module Module exposing
 	(Msg, Module)

import Panel exposing (..)

type Module
    = Model Internals

type alias Internals =
	{side : Panel}




type Msg
	= ChgView
