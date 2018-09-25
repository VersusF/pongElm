module Main exposing (Model, Msg(..), main)

import Browser
import Html as H exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events as H exposing (..)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Time
import Html.Events.Extra.Mouse as Mouse
import Random

import Ball exposing (Ball)
import Square exposing (Square)
import Bouncer exposing (Bouncer)
import PlayerStatus exposing (PlayerStatus)


type alias Model =
  { ball : Ball
  , square : Square
  , bouncer : Bouncer
  , playerStatus : PlayerStatus
  }

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type Msg
  = CreateBall Int
  | UpdateBall Time.Posix
  | UpdateBouncer Mouse.Event
  | Start


init : () -> (Model, Cmd Msg)
init () =
  let
    square = Square.initialSquare
  in
    ( Model Ball.none square (Bouncer.initialBouncer square) PlayerStatus.initialPlayerStatus
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    framerate = 30
    refreshTimeMillis = 1000 / framerate
  in
  Sub.batch
    [ Time.every refreshTimeMillis UpdateBall
    ]


view : Model -> Html Msg
view model =
  let
    widthString = String.fromInt model.square.width
    heightString = String.fromInt model.square.height
    svgViewBox = "0 0 " ++ widthString ++ " " ++ heightString
  in
    div 
    [ H.style "text-align" "center"
    , H.style "width" "100%"
    ]
    [ svg 
        [ S.width widthString
        , S.height heightString
        , S.style "cursor:none"
        , viewBox svgViewBox
        , Mouse.onMove UpdateBouncer
        , H.onClick Start
        ]
        [ Square.drawSquare model.square
        , Ball.drawBall model.ball
        , Bouncer.drawBouncer model.bouncer
        ]
    , drawStartText model.playerStatus
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateBall x ->
      ( {model | ball = Ball.randomBall x}
      , Cmd.none
      )

    Start ->
      ( {model | playerStatus = PlayerStatus.startGame}
      , Random.generate CreateBall (Random.int 0 model.square.width)
      )

    UpdateBall _ ->
      let
        (newBall, newStatus) = Ball.updateBall model.ball model.square model.bouncer model.playerStatus
      in
        ( { model | ball = newBall, playerStatus = newStatus}
        , Cmd.none
        )

    UpdateBouncer event ->
      let
        xPos = Tuple.first event.offsetPos
          |> round
          |> (+) (model.bouncer.width // -2)
      in
        ( {model | bouncer = Bouncer.updateBouncer model.bouncer xPos}
        , Cmd.none
        )

drawStartText : PlayerStatus -> Html Msg
drawStartText playerStatus =
  if PlayerStatus.isGameOver playerStatus || PlayerStatus.hasNotStart playerStatus then
    p [] [H.text "Click anywhere to start"]
  else
    p [] []