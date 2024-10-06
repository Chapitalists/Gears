module Tools.Panel exposing
    ( Side(..)
    , ViewType(..)
    , view
    )

import Element exposing (..)
import Element.Background as Bg
import Element.Border as B
import Html.Events as E
import Palette exposing (..)
import Tools.Utils exposing (Size)


type ViewType
    = Hidden
    | Border { side : Side, size : Size }
    | Full


type Side
    = Top
    | Bottom
    | Left
    | Right


view :
    ( ViewType -> msg
    , ViewType -> Element msg
    )
    -> Side
    -> ViewType
    -> Size
    -> Attribute msg
view ( panelMsg, subView ) side viewType size =
    let
        kit =
            case side of
                Top ->
                    { alignSide = alignTop
                    , alignAnti = alignBottom
                    , center = centerX
                    , arrowOut = arrowDown
                    , arrowIn = arrowUp
                    , attr = width <| px size.width
                    , layout = column
                    , arrowFirst = False
                    }

                Bottom ->
                    { alignSide = alignBottom
                    , alignAnti = alignTop
                    , center = centerX
                    , arrowOut = arrowUp
                    , arrowIn = arrowDown
                    , attr = width <| px size.width
                    , layout = column
                    , arrowFirst = True
                    }

                Right ->
                    { alignSide = alignRight
                    , alignAnti = alignLeft
                    , center = centerY
                    , arrowOut = arrowLeft
                    , arrowIn = arrowRight
                    , attr = height <| px size.height
                    , layout = row
                    , arrowFirst = True
                    }

                Left ->
                    { alignSide = alignLeft
                    , alignAnti = alignRight
                    , center = centerY
                    , arrowOut = arrowRight
                    , arrowIn = arrowLeft
                    , attr = height <| px (size.height - marginBase * 2)
                    , layout = row
                    , arrowFirst = False
                    }
    in
    case viewType of
        Hidden ->
            inFront <|
                el
                    ((htmlAttribute <| E.onClick <| panelMsg <| Border)
                        :: [ kit.alignSide, kit.center, padding 5 ]
                    )
                    (text <| String.fromChar kit.arrowOut)

        Border ->
            let
                arrow =
                    el
                        ((htmlAttribute <| E.onClick <| panelMsg <| Hidden)
                            :: [ kit.center, padding 5 ]
                        )
                        (text <| String.fromChar kit.arrowIn)
            in
            inFront <|
                kit.layout
                    [ kit.alignSide
                    , kit.attr
                    , kit.center
                    , B.roundEach
                        { topRight = roundBase
                        , bottomRight = roundBase
                        , topLeft = 0
                        , bottomLeft = 0
                        }
                    , B.widthEach
                        { top = strokeBase
                        , bottom = strokeBase
                        , right = strokeBase
                        , left = 0
                        }
                    , B.color <| toEl borderBase
                    , Bg.color <| toEl bgBase
                    ]
                <|
                    if kit.arrowFirst then
                        [ arrow, subView ]

                    else
                        [ subView, arrow ]


arrowDown : Char
arrowDown =
    '˅'


arrowUp : Char
arrowUp =
    '˄'


arrowLeft : Char
arrowLeft =
    '˂'


arrowRight : Char
arrowRight =
    '˃'
