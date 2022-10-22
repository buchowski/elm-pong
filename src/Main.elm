module Main exposing (..)

import Browser
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program () Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { playerOnePosY : Int
    , playerTwoPosY : Int
    , ballPosX : Int
    , ballPosY : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 50 50 300 100
    , Cmd.none
    )


type Msg
    = MovePlayerOne Int
    | MovePlayerTwo Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MovePlayerOne posY ->
            ( { model | playerOnePosY = posY }
            , Cmd.none
            )

        MovePlayerTwo posY ->
            ( { model | playerTwoPosY = posY }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "stimpy's elm page"
    , body = [ backdrop model ]
    }


backdrop : Model -> Svg msg
backdrop model =
    svg
        [ viewBox "0 0 600 400"
        , width "600"
        , height "400"
        ]
        [ rect [ width "100%", height "100%" ] []
        , paddleOne model.playerOnePosY
        , paddleTwo model.playerTwoPosY
        , ball model.ballPosX model.ballPosY
        ]


ball : Int -> Int -> Svg msg
ball posX posY =
    circle [ fill "white", r "12", cx (String.fromInt posX), cy (String.fromInt posY) ] []


paddleOne : Int -> Svg msg
paddleOne yPos =
    paddle "red" 40 yPos


paddleTwo : Int -> Svg msg
paddleTwo yPos =
    paddle "blue" 540 yPos


paddle : String -> Int -> Int -> Svg msg
paddle color xPosInt yPosInt =
    let
        xPos =
            String.fromInt xPosInt

        yPos =
            String.fromInt yPosInt
    in
    rect
        [ fill color, x xPos, y yPos, width "20", height "45" ]
        []
