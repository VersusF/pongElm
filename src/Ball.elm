module Ball exposing
  (Ball
  , startPosition
  , updateBall
  , drawBall
  )

import Square exposing (Square)
import Bouncer exposing (Bouncer)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Ball =
  { x : Int
  , y : Int
  , speedX : Int
  , speedY : Int
  }

startPosition : Ball 
startPosition =
  Ball 200 300 3 3

updateBall : Ball -> Square -> Bouncer -> Ball
updateBall ball square bouncer =
  let
    (newX, newSpeedX) = nextPositionAndSpeed ball.x ball.speedX 0 square.width
    upperY =
      if ball.x > bouncer.x && ball.x < (bouncer.x + bouncer.width) then
        bouncer.y
      else
        square.height
    (newY, newSpeedY) = nextPositionAndSpeed ball.y ball.speedY 0 upperY
  in
    Ball newX newY newSpeedX newSpeedY
    

nextPositionAndSpeed : Int -> Int -> Int -> Int -> (Int, Int)
nextPositionAndSpeed pos speed lower upper =
  let
    possiblePos = pos + speed
  in
    if possiblePos < lower then
      (lower, negate speed)
    else if possiblePos > upper then
      (upper, negate speed)
    else
      (possiblePos, speed)

drawBall : Ball -> Svg a
drawBall ball =
  circle
    [ cx <| String.fromInt ball.x
    , cy <| String.fromInt ball.y
    , r "2"
    ]
    []