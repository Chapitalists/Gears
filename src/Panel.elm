module Panel exposing (..)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as B
import Html.Events as E
import Palette exposing (..)


type alias BoxSize =
    { width : Int, height : Int }


type alias Panel =
    { state : State
    , side : Side
    , size : Int
    }


type State
    = Hidden
    | Shown


type Side
    = Top
    | Bottom
    | Left
    | Right


type Msg
    = ChgState State


update msg panel =
    case msg of
        ChgState state ->
            { panel | state = state }


view :
    (Msg -> msg)
    -> BoxSize
    -> Panel
    -> Element msg
    -> Attribute msg
view panelMsg size p subView =
    let
        kit =
            case p.side of
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
    case p.state of
        Hidden ->
            inFront <|
                el
                    ((htmlAttribute <| E.onClick <| panelMsg <| ChgState Shown)
                        :: [ kit.alignSide, kit.center, padding 5 ]
                    )
                    (text <| String.fromChar kit.arrowOut)

        Shown ->
            let
                arrow =
                    el
                        ((htmlAttribute <| E.onClick <| panelMsg <| ChgState Hidden)
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
