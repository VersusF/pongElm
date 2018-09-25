module Ball exposing
  (Ball
  , randomBall
  , updateBall
  , drawBall
  , startBall
  , none
  )

import Square exposing (Square)
import Bouncer exposing (Bouncer)
import PlayerStatus exposing (PlayerStatus)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Ball =
  { x : Int
  , y : Int
  , speedX : Int
  , speedY : Int
  }

none : Ball
none =
  Ball 0 0 0 0

randomBall : Int -> Ball
randomBall x =
  Ball x 0 0 0
    |> startBall

startBall : Ball -> Ball
startBall ball = {ball | speedX = 3, speedY = 3}

updateBall : Ball -> Square -> Bouncer -> PlayerStatus -> (Ball, PlayerStatus)
updateBall ball square bouncer previousState =
  let
    (newX, speedX) = nextPositionAndSpeed ball.x ball.speedX 0 square.width
    upperY =
      if ball.x >= bouncer.x 
        && ball.x <= (bouncer.x + bouncer.width)
        && ball.y <= bouncer.y 
      then
        bouncer.y
      else
        square.height
    (newY, speedY) = nextPositionAndSpeed ball.y ball.speedY 0 upperY
    (newState, newSpeedX, newSpeedY) = 
      if newY == square.height then
        (PlayerStatus.gameOver, 0, 0)
      else
        (previousState, speedX, speedY)
    (incX, incY) = 
      if newY == bouncer.y then
        (findIncrement newSpeedX, findIncrement newSpeedY)
      else
        (0, 0)
  in
    (Ball newX newY (newSpeedX + incX) (newSpeedY + incY), newState)
    

findIncrement : Int -> Int
findIncrement n =
  if n > 0 then 1 else -1


nextPositionAndSpeed : Int -> Int -> Int -> Int -> (Int, Int)
nextPositionAndSpeed pos speed lower upper =
  let
    possiblePos = pos + speed
  in
    if possiblePos < lower then
      (lower, negate speed)
    else if possiblePos > upper then
      Debug.log "boing" (upper, negate speed)
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