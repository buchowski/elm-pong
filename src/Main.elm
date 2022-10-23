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
    , ballDeltaX : Int
    , ballDeltaY : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model startYPos startYPos 300 100 xSpeed -ySpeed
    , Cmd.none
    )


type Msg
    = MovePlayerOneUp
    | MovePlayerOneDown
    | MovePlayerTwoUp
    | MovePlayerTwoDown
    | MoveBall Float
    | Other


moveDelta : Int
moveDelta =
    10


startYPos : Int
startYPos =
    maxYPos // 2


maxYPos : Int
maxYPos =
    400 - 45 - 10


getPosInBounds : Int -> Int
getPosInBounds posY =
    if posY >= maxYPos then
        maxYPos

    else if posY <= 10 then
        10

    else
        posY


xSpeed : Int
xSpeed =
    5


ySpeed : Int
ySpeed =
    2


getIsCollision : Model -> Int -> Int -> Bool
getIsCollision model paddlePosX paddlePosY =
    let
        ballLeft =
            model.ballPosX - ballRadius

        ballRight =
            model.ballPosX + ballRadius

        ballTop =
            model.ballPosY - ballRadius

        ballBottom =
            model.ballPosY + ballRadius

        paddleLeft =
            paddlePosX

        paddleRight =
            paddlePosX + paddleWidth

        paddleTop =
            paddlePosY

        paddleBottom =
            paddlePosY + paddleHeight

        doesBallLeftOverlap =
            ballLeft >= paddleLeft && ballLeft <= paddleRight

        doesBallRightOverlap =
            ballRight >= paddleLeft && ballRight <= paddleRight

        doesBallTopOverlap =
            ballTop >= paddleTop && ballTop <= paddleBottom

        doesBallBottomOverlap =
            ballBottom >= paddleTop && ballBottom <= paddleBottom
    in
    (doesBallLeftOverlap || doesBallRightOverlap) && (doesBallTopOverlap || doesBallBottomOverlap)


getNextYDelta : Model -> Int
getNextYDelta model =
    let
        isTooHigh =
            model.ballPosY <= 0

        isTooLow =
            model.ballPosY >= 400
    in
    if isTooHigh then
        ySpeed

    else if isTooLow then
        -ySpeed

    else
        model.ballDeltaY


getNextXDelta : Model -> Int
getNextXDelta model =
    let
        isTooFarRight =
            model.ballPosX >= 600

        isTooFarLeft =
            model.ballPosX <= 0

        isLeftPaddleCollison =
            getIsCollision model paddleOneXPos model.playerOnePosY

        isRightPaddleCollison =
            getIsCollision model paddleTwoXPos model.playerTwoPosY

        isCollision =
            isLeftPaddleCollison || isRightPaddleCollison
    in
    if isCollision then
        -model.ballDeltaX

    else if isTooFarRight then
        -xSpeed

    else if isTooFarLeft then
        xSpeed

    else
        model.ballDeltaX


getNextDeltas : Model -> ( Int, Int )
getNextDeltas model =
    ( getNextXDelta model, getNextYDelta model )


updateModelWithNewBallPos : Model -> Model
updateModelWithNewBallPos model =
    let
        ( xDelta, yDelta ) =
            getNextDeltas model
    in
    { model | ballPosX = model.ballPosX + xDelta, ballDeltaX = xDelta, ballPosY = model.ballPosY + yDelta, ballDeltaY = yDelta }


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

        MoveBall _ ->
            ( updateModelWithNewBallPos model, Cmd.none )

        Other ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            keyDecoder
        , Browser.Events.onAnimationFrameDelta
            MoveBall
        ]


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


ballRadius : Int
ballRadius =
    12


ball : Int -> Int -> Svg msg
ball posX posY =
    circle [ fill "white", r (String.fromInt ballRadius), cx (String.fromInt posX), cy (String.fromInt posY) ] []


paddleOneXPos : Int
paddleOneXPos =
    40


paddleTwoXPos : Int
paddleTwoXPos =
    540


paddleOne : Int -> Svg msg
paddleOne yPos =
    paddle "red" paddleOneXPos yPos


paddleTwo : Int -> Svg msg
paddleTwo yPos =
    paddle "skyblue" paddleTwoXPos yPos


paddleWidth : Int
paddleWidth =
    20


paddleHeight : Int
paddleHeight =
    45


paddle : String -> Int -> Int -> Svg msg
paddle color xPosInt yPosInt =
    let
        xPos =
            String.fromInt xPosInt

        yPos =
            String.fromInt yPosInt
    in
    rect
        [ fill color, x xPos, y yPos, width (String.fromInt paddleWidth), height (String.fromInt paddleHeight) ]
        []
