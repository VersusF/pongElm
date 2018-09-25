module Bouncer exposing 
  ( Bouncer
  , initialBouncer
  , drawBouncer
  , updateBouncer
  )

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Square exposing (Square)

type alias Bouncer =
  { height : Int
  , width : Int
  , x : Int
  , y : Int
  }

initialBouncer : Square -> Bouncer
initialBouncer square =
  Bouncer 5 30 (square.width // 2) square.bouncerHeight

drawBouncer : Bouncer -> Svg a
drawBouncer bouncer =
  rect 
  [ width <| String.fromInt bouncer.width
  , height <| String.fromInt bouncer.height
  , x <| String.fromInt bouncer.x
  , y <| String.fromInt bouncer.y
  , Svg.Attributes.style "fill-opacity:1"
  ]
  []

updateBouncer : Bouncer -> Int -> Bouncer
updateBouncer bouncer newPosX =
  {bouncer | x = newPosX}