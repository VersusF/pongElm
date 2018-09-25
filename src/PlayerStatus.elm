module PlayerStatus exposing (..)

type PlayerStatus 
  = Playing
  | GameOver
  | NotYetStarted

initialPlayerStatus : PlayerStatus
initialPlayerStatus = NotYetStarted

continuePlaying : PlayerStatus
continuePlaying = Playing

gameOver : PlayerStatus
gameOver = GameOver

hasNotStart : PlayerStatus -> Bool
hasNotStart playerStatus = playerStatus == NotYetStarted 

startGame : PlayerStatus
startGame = Playing

isGameOver : PlayerStatus -> Bool
isGameOver playerStatus = playerStatus == GameOver

isPlaying : PlayerStatus -> Bool
isPlaying playerStatus = playerStatus == Playing