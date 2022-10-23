module Main exposing (..)

import Browser
import Browser.Events exposing (..)
import Html exposing (..)
import Json.Decode as Decode
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
    = MovePlayerOneUp
    | MovePlayerOneDown
    | MovePlayerTwoUp
    | MovePlayerTwoDown
    | Other


moveDelta : Int
moveDelta =
    10


getPosInBounds : Int -> Int
getPosInBounds posY =
    if posY > 400 then
        400

    else if posY < 0 then
        0

    else
        posY


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MovePlayerOneUp ->
            ( { model | playerOnePosY = getPosInBounds model.playerOnePosY - moveDelta }
            , Cmd.none
            )

        MovePlayerOneDown ->
            ( { model | playerOnePosY = getPosInBounds model.playerOnePosY + moveDelta }
            , Cmd.none
            )

        MovePlayerTwoUp ->
            ( { model | playerTwoPosY = getPosInBounds model.playerTwoPosY - moveDelta }
            , Cmd.none
            )

        MovePlayerTwoDown ->
            ( { model | playerTwoPosY = getPosInBounds model.playerTwoPosY + moveDelta }
            , Cmd.none
            )

        Other ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKeyboardInput (Decode.field "key" Decode.string)


toKeyboardInput : String -> Msg
toKeyboardInput key =
    case key of
        "w" ->
            MovePlayerOneUp

        "s" ->
            MovePlayerOneDown

        "ArrowUp" ->
            MovePlayerTwoUp

        "ArrowDown" ->
            MovePlayerTwoDown

        _ ->
            Other


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
