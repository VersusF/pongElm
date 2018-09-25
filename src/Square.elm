module Square exposing (Square, initialSquare, drawSquare)

import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Square =
  { width : Int
  , height : Int
  , bouncerHeight : Int
  }

initialSquare : Square
initialSquare =
  Square 600 400 370

drawSquare : Square -> Svg a
drawSquare square = 
  rect 
    [ width <| String.fromInt square.width
    , height <| String.fromInt square.height
    , Svg.Attributes.style "stroke:black; fill-opacity:0"
    ]
    []