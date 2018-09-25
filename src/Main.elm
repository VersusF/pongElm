module Main exposing (Model, Msg(..), main)

import Browser
import Html exposing (..)
import Html.Attributes as H exposing (..)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Time
import Html.Events.Extra.Mouse as Mouse

import Ball exposing (Ball)
import Square exposing (Square)
import Bouncer exposing (Bouncer)


type alias Model =
  { ball : Ball
  , square : Square
  , bouncer : Bouncer
  }

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type Msg
  = Init
  | UpdateBall Time.Posix
  | UpdateBouncer Mouse.Event


init : () -> (Model, Cmd Msg)
init () =
  let
    square = Square.initialSquare
  in
    ( Model Ball.startPosition square (Bouncer.initialBouncer square)
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    framerate = 60
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
    [      
      svg 
        [ S.width widthString
        , S.height heightString
        , viewBox svgViewBox
        , Mouse.onMove UpdateBouncer
        ]
        [ Square.drawSquare model.square
        , Ball.drawBall model.ball
        , Bouncer.drawBouncer model.bouncer
        ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Init ->
      ( model
      , Cmd.none
      )

    UpdateBall _ ->
      ( { model | ball = Ball.updateBall model.ball model.square model.bouncer}
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
