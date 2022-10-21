module Main exposing (..)
import Browser
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main : Program () Model Msg
main = Browser.document { init = init, view = view, update = update, subscriptions = subscriptions}

type alias Model = {
  playerOnePosY: Int,
  playerTwoPosY: Int
  }

init : () -> (Model, Cmd Msg)
init _ =  
  ( Model 50 50
  , Cmd.none)

type Msg
  = MovePlayerOne Int
  | MovePlayerTwo Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MovePlayerOne posY ->
      ( { model | playerOnePosY = posY}
      , Cmd.none)
    MovePlayerTwo posY ->
      ( { model | playerTwoPosY = posY}
      , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Browser.Document Msg
view _ = {
  title = "stimpy's elm page",
  body = [backdrop]
  }

backdrop : Svg msg
backdrop = svg
      [ viewBox "0 0 400 600"
      , width "600"
      , height "400"
      ]
      [rect [width "100", height "100"] []]
